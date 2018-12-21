module ReactCsv

open Fable.Core
open Fable.Import.React
open Fable.Helpers
open Fable.Helpers.React
open Fable.Core.JsInterop

type Header =
 { label : string; key : string}
 
type ICsvLinkProps = interface end
type ICsvDownloadProps = interface end

type CommonProps =
    | Data of System.Array
    | Headers of Header array
    | Separator of string
    
    interface ICsvLinkProps
    interface ICsvDownloadProps

type CsvLinkProps =
    | Filename of string
    | ClassName of string
    | AsyncOnClick of bool
    | OnClick of U2<obj -> bool, (obj * bool -> unit) -> unit>
    interface ICsvLinkProps
    
type CsvDownloadProps =
    | Target of string
    interface ICsvDownloadProps

let inline csvLink (props : ICsvLinkProps list) (elems : ReactElement list) : ReactElement =
    ofImport "CSVLink" "react-csv" (keyValueList CaseRules.LowerFirst props) elems

let inline csvDownload (props : ICsvDownloadProps list) (elems : ReactElement list) : ReactElement =
    ofImport "CSVDownload" "react-csv" (keyValueList CaseRules.LowerFirst props) elems
