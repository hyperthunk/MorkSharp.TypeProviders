namespace MorkSharp.TypeProviders

open System
open System.IO
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open VDS.RDF
open VDS.RDF.Parsing

/// The provided record type for each property
type MorkPropertyRecord = { Name: string; Iri: string }

[<TypeProvider>]
type MorkPropertyProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)
    let ns = "MorkSharp.TypeProviders"
    let asm = Assembly.GetExecutingAssembly()

    let makeValidCaseName (s:string) =
        let s = s.TrimStart(':', '#')
        let s = s.Replace("-", "_").Replace(".", "_")
        let s = if System.Char.IsDigit(s.[0]) then "_" + s else s
        s.Substring(0,1).ToUpper() + s.Substring(1)

    /// Try to get the embedded resource stream for Mork.ttl
    let getEmbeddedTtlStream() =
        let resourceName =
            asm.GetManifestResourceNames()
            |> Array.tryFind (fun n -> n.EndsWith("Mork.ttl", StringComparison.OrdinalIgnoreCase))
        match resourceName with
        | Some name -> asm.GetManifestResourceStream(name)
        | None -> failwith "Embedded Mork.ttl not found in assembly resources. Ensure Mork.ttl is set as an embedded resource."

    do
        // Parse ontology from embedded resource
        let g = new Graph()
        use stream = getEmbeddedTtlStream()
        let ttl = new TurtleParser()
        ttl.Load(g, new StreamReader(stream))

        let objectProps =
            g.GetTriplesWithPredicateObject(
                g.CreateUriNode("rdf:type"),
                g.CreateUriNode("owl:ObjectProperty"))
            |> Seq.map (fun t -> t.Subject)
            |> Seq.choose (function
                | :? IUriNode as uri -> Some uri.Uri.AbsoluteUri
                | _ -> None)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toList

        // Provided erased record type: MorkProperty
        let morkPropTy = ProvidedTypeDefinition(asm, ns, "MorkProperty", Some typeof<MorkPropertyRecord>, hideObjectMethods=true)

                // Add static properties for each property, returning the erased record
        for iri in objectProps do
            let label =
                match iri.Split([|'#'|], StringSplitOptions.RemoveEmptyEntries) with
                | [| |] -> failwithf "Malformed IRI: %s" iri
                | parts -> makeValidCaseName parts.[parts.Length-1]

            let prop = 
                ProvidedProperty(
                    label,
                    morkPropTy,
                    isStatic = true,
                    getterCode = (fun _ -> 
                        let recVal = { Name = label; Iri = iri }
                        <@@ recVal @@>))
            prop.AddXmlDoc(sprintf "IRI: %s" iri)
            morkPropTy.AddMember(prop)

        // Add instance properties: Name, Iri
        let nameProp = ProvidedProperty("Name", typeof<string>, getterCode = (fun [this] -> <@@ (%%this : MorkPropertyRecord).Name @@>))
        let iriProp = ProvidedProperty("Iri", typeof<string>, getterCode = (fun [this] -> <@@ (%%this : MorkPropertyRecord).Iri @@>))
        morkPropTy.AddMember(nameProp)
        morkPropTy.AddMember(iriProp)

        // Add a constructor for custom property (for unknown/custom IRIs)
        let ctor = ProvidedMethod(
            methodName = "CreateCustom",
            parameters = [ ProvidedParameter("iri", typeof<string>) ],
            returnType = morkPropTy,
            isStatic = true,
            invokeCode = (fun [iriExpr] ->
                <@@ { Name = "CustomProperty"; Iri = %%iriExpr } @@>
            )
        )
        ctor.AddXmlDoc("Create a MorkProperty for an arbitrary IRI.")
        morkPropTy.AddMember(ctor)

        // Add instance properties: Name, Iri
        let nameProp = ProvidedProperty("Name", typeof<string>, getterCode = (fun [this] -> <@@ (%%this :> MorkPropertyRecord).Name @@>))
        let iriProp = ProvidedProperty("Iri", typeof<string>, getterCode = (fun [this] -> <@@ (%%this :> MorkPropertyRecord).Iri @@>))
        morkPropTy.AddMember(nameProp)
        morkPropTy.AddMember(iriProp)

        // Add static property All for enumeration
        let allPropsExpr =
            let vals = 
                objectProps
                |> List.map (fun iri ->
                    let label = iri.Split([|'#'|], StringSplitOptions.RemoveEmptyEntries) |> Array.last |> makeValidCaseName
                    { Name = label; Iri = iri })
                |> List.toArray
            <@@ vals @@>
        let allProp = ProvidedProperty("All", typeof<MorkPropertyRecord[]>, isStatic = true, getterCode = (fun _ -> allPropsExpr))
        allProp.AddXmlDoc("All object properties from the Mork ontology.")
        morkPropTy.AddMember(allProp)

        // Add to namespace
        this.AddNamespace(ns, [morkPropTy])

[<assembly:TypeProviderAssembly>]
do ()