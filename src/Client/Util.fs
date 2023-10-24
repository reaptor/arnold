module Util

open Shared

let pair x y = x, y

module RepositoryPath =
    let asUriEncoded (RepositoryPath p) = Fable.Core.JS.encodeURIComponent p

    let ofUriEncoded p =
        Fable.Core.JS.decodeURIComponent p |> RepositoryPath
