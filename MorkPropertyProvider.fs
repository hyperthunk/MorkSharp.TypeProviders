#nowarn 59
(* Sadly in-keeping with Java's approach, the .NET type system is somewhat unsound.
   Consider, for example: x'.[0] <- upcast [| "test" |] // throws runtime System.ArrayTypeMismatchException

   We do not, however, need to worry about the coersion taking place in properties we are 
   explicitly binding to a particular type when generating code, viz `this :> MorkPropertyRecord`
*)

namespace MorkSharp.TypeProviders

open System
open System.IO
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open VDS.RDF
open VDS.RDF.Ontology
open VDS.RDF.Parsing

/// The provided record type for each property
type MorkPropertyRecord = { Name: string; Iri: string }

[<TypeProvider>]
type public MorkPropertyProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)
    let ns = "MorkSharp.TypeProviders"
    let asm = Assembly.GetExecutingAssembly()

    /// Try to get the embedded resource stream for Mork.ttl
    let getEmbeddedTtlStream() =
        let resourceName =
            asm.GetManifestResourceNames()
            |> Array.tryFind (fun n -> n.EndsWith("Mork.ttl", StringComparison.OrdinalIgnoreCase))
        match resourceName with
        | Some name -> asm.GetManifestResourceStream(name)
        | None -> failwith "Embedded Mork.ttl not found in assembly resources. Ensure Mork.ttl is set as an embedded resource."

    let morkBaseURI = "http://www.nebularis.org/ontologies/Mork"
    let skosBaseURI = "http://www.w3.org/2004/02/skos/core#"

    let mkValidName (s:string) =
        let s = s.TrimStart(':', '#')
        let s = s.Replace("-", "_").Replace(".", "_")
        let s = if System.Char.IsDigit(s.[0]) then "_" + s else s
        s.Substring(0,1).ToUpper() + s.Substring(1)

    let mkValidCaseName (ss: string array) = 
        match ss with
            | [|baseUri; rest|] 
                when skosBaseURI.StartsWith(baseUri) -> mkValidName ("Skos" + rest)
            | [|_; rest|] -> mkValidName rest
            | parts when parts.Length > 1 -> mkValidName parts.[parts.Length-1]
            | _ -> failwithf "Malformed IRI: %A" ss

    let mapPropertyies (sequence: seq<INode>) = 
        sequence 
            |> Seq.choose (function
                | :? IUriNode as uri
                    when uri.Uri.AbsoluteUri.StartsWith(morkBaseURI) ||
                        uri.Uri.AbsoluteUri.StartsWith(skosBaseURI) -> 
                        Some uri.Uri.AbsoluteUri
                | _ -> None) 
            |> Seq.distinct
            |> Seq.sort

    do
        // Parse ontology from embedded resource
        let g = new OntologyGraph()
        use stream = getEmbeddedTtlStream()
        let ttl = new TurtleParser()
        ttl.Load(g, new StreamReader(stream))

        let objectProps = 
            g.OwlObjectProperties |> Seq.map (fun t -> t.Resource) |> mapPropertyies
        let objectAndDataProps = 
            g.OwlDatatypeProperties 
                |> Seq.map (fun t -> t.Resource) 
                |> mapPropertyies
                |> Seq.append objectProps

        // Provided erased record type: MorkProperty
        let morkPropTy = 
            ProvidedTypeDefinition(asm, ns, "MorkProperty", 
                                   Some typeof<MorkPropertyRecord>, 
                                   hideObjectMethods=true)

                // Add static properties for each property, returning the erased record
        let typeDefs: list<ProvidedTypeDefinition> = 
            objectAndDataProps 
                |> Seq.map (fun iri ->
                    // static accessor for the generated type
                    let label = mkValidCaseName <| iri.Split([|'#'|], StringSplitOptions.RemoveEmptyEntries)
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

                    let morkInstanceTy = 
                        ProvidedTypeDefinition(asm, ns, $"Mork{label}Prop", 
                                            Some typeof<MorkPropertyRecord>, 
                                            hideObjectMethods=true)
                    (* 
                        Frustratingly, adding no-warn here for the match expression in the lambda for the 
                        getterCode will disable incomplete match warnings for the the whole file! :/ 
                    *)
                    let nameProp = 
                        ProvidedProperty("Name", 
                                        typeof<string>, 
                                        getterCode = (fun [this] -> <@@ label @@>))
                    let iriProp = 
                        ProvidedProperty("Iri", 
                                        typeof<string>, 
                                        getterCode = (fun [this] -> <@@ iri @@>))
                    
                    morkInstanceTy.AddMember nameProp
                    morkInstanceTy.AddMember iriProp
                    morkInstanceTy
                ) |> Seq.toList

        // TODO: consider whether we _really_ need this?
        // generate { member this.CreateCustom (iri: string): MorkPropertyRecord }
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
        
        // generate {member this.AllProperties: seq<MorkPropertyRecord>}
        let allPropsExpr =
            let vals = 
                objectProps
                |> Seq.map (fun iri ->
                    let label = mkValidCaseName <| iri.Split([|'#'|], StringSplitOptions.RemoveEmptyEntries)
                    in { Name = label; Iri = iri }
                )
            <@@ vals @@>
        
        let allProp = ProvidedProperty("AllProperties", typeof<seq<MorkPropertyRecord>>, isStatic = true, getterCode = (fun _ -> allPropsExpr))
        allProp.AddXmlDoc("Complete set of properties from the Mork and Skos ontologies.")
        morkPropTy.AddMember(allProp)

        // Add to namespace
        this.AddNamespace(ns, morkPropTy :: typeDefs)

[<assembly:TypeProviderAssembly>]
do ()