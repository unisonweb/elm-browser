module Ucb.Main.Message exposing (..)

import Browser
import Bytes exposing (Bytes)
import HashingContainers.HashSet exposing (HashSet)
import Ucb.Unison.BranchDict exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Url


type Message
    = User_FocusBranch BranchHash
    | User_GetPatches BranchHash
    | User_ToggleBranch BranchHash
    | User_ToggleTerm Id
    | User_Search String
    | Http_GetBranch
        (Result (Http.Error Bytes)
            ( BranchHash
            , { branches : BranchDict Branch
              , parents : BranchDict (HashSet BranchHash)
              , successors : BranchDict (HashSet BranchHash)
              }
            )
        )
    | Http_GetHeadHash (Result (Http.Error String) (Http.Response BranchHash))
    | Http_GetPatches (Result (Http.Error Bytes) (List ( PatchHash, Patch )))
    | Http_GetTerm (Result (Http.Error Bytes) ( Id, Http.Response (Term Symbol) ))
    | Http_GetTermTypesAndTypeDecls (Result (Http.Error Bytes) ( List ( Id, Type Symbol ), List ( Id, Declaration Symbol ) ))
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
