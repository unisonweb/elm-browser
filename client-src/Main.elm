module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (..)
import Task
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (..)
import Ucb.Main.View exposing (viewModel)
import Ucb.Unison.BranchDict as BranchDict exposing (BranchDict)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Unison.Codebase.API.GitHub exposing (..)
import Ucb.Unison.Codebase.API.LocalServer exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Url
import Util.HashSet as HashSet


main : Program () Model Message
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewModel
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


{-| Port that elm-live runs on
-}
elmLivePort : number
elmLivePort =
    8000


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url key =
    let
        isDevMode : Bool
        isDevMode =
            case url.port_ of
                Nothing ->
                    False

                Just portNum ->
                    portNum == elmLivePort

        model : Model
        model =
            { api =
                { unison = makeLocalServerUnisonCodebaseAPI isDevMode
                }
            , codebase =
                { head = Nothing
                , branches = BranchDict.empty
                , patches = HashDict.empty hash32Equality hash32Hashing
                , terms = HashDict.empty idEquality idHashing
                , termTypes = HashDict.empty idEquality idHashing
                , typeDecls = HashDict.empty idEquality idHashing
                , parents = BranchDict.empty
                , successors = BranchDict.empty
                }
            , ui =
                { branch = []
                , terms = HashDict.empty idEquality idHashing
                , search = ""
                , hovered = Nothing
                , key = key
                }
            , errors = []
            , isDevMode = isDevMode
            }

        -- First command: fetch _head path!
        initialCommand : Cmd Message
        initialCommand =
            model.api.unison.getHeadHash
                |> Task.attempt Http_GetHeadHash
    in
    ( model, initialCommand )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Http_GetHeadHash result ->
            update_Http_GetHeadHash result model

        Http_GetBranch result ->
            update_Http_GetBranch result model

        Http_GetPatches result ->
            update_Http_GetPatches result model

        Http_GetTerm result ->
            update_Http_GetTerm result model

        Http_GetTermTypesAndTypeDecls result ->
            update_Http_GetTermTypesAndTypeDecls result model

        User_ClickBranch path branch ->
            update_User_ClickBranch path branch model

        User_FocusBranch hash ->
            update_User_FocusBranch hash model

        User_GetPatches hash ->
            update_User_GetPatches hash model

        User_Hover hover ->
            update_User_HoverTerm hover model

        User_Unhover ->
            update_User_Unhover model

        User_Search search ->
            update_User_Search search model

        User_ToggleTerm id ->
            update_User_ToggleTerm id model

        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| Got the head hash. Next step: get the actual (decoded) bytes.
-}
update_Http_GetHeadHash :
    Result (Http.Error String) (Http.Response BranchHash)
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetHeadHash result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetHeadHash err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            update_Http_GetHeadHash2 response model


update_Http_GetHeadHash2 :
    Http.Response BranchHash
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetHeadHash2 response model =
    let
        command : Cmd Message
        command =
            getBranch
                model.api.unison
                model.codebase
                response.body
                |> Task.attempt Http_GetBranch
    in
    ( model
    , command
    )


update_Http_GetBranch :
    Result (Http.Error Bytes)
        ( BranchHash
        , { branches : BranchDict Branch
          , parents : BranchDict (HashSet BranchHash)
          , successors : BranchDict (HashSet BranchHash)
          }
        )
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetBranch result model =
    case result of
        Err err ->
            ( { model
                | errors = Err_GetBranch err :: model.errors
              }
            , Cmd.none
            )

        Ok result2 ->
            update_Http_GetBranch2 result2 model


update_Http_GetBranch2 :
    ( BranchHash
    , { branches : HashDict BranchHash Branch
      , parents : HashDict BranchHash (HashSet BranchHash)
      , successors : HashDict BranchHash (HashSet BranchHash)
      }
    )
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetBranch2 ( hash, { branches, parents, successors } ) model =
    let
        updateCodebase :
            ModelCodebase
            -> ModelCodebase
        updateCodebase codebase =
            let
                newBranches : BranchDict Branch
                newBranches =
                    HashDict.foldl
                        (\( hash_, branch ) ->
                            HashDict.update
                                hash_
                                (maybe (Just branch) Just)
                        )
                        codebase.branches
                        branches

                newParents : BranchDict (HashSet BranchHash)
                newParents =
                    (BranchDict.monoid HashSet.semigroup).semigroup.prepend
                        parents
                        codebase.parents

                newSuccessors : BranchDict (HashSet BranchHash)
                newSuccessors =
                    (BranchDict.monoid HashSet.semigroup).semigroup.prepend
                        successors
                        codebase.successors
            in
            { codebase
                | -- We assume that because we fetched this branch, we wanted to
                  -- focus it.
                  head = Just hash
                , branches = newBranches
                , parents = newParents
                , successors = newSuccessors
            }

        updateUI :
            ModelUI
            -> ModelUI
        updateUI ui =
            { ui
                | branch = []
            }

        newModel : Model
        newModel =
            { model
                | codebase = updateCodebase model.codebase
                , ui = updateUI model.ui
            }

        -- Fetch missing term types and type decls for the branch we just
        -- fetched
        command : Cmd Message
        command =
            case HashDict.get hash newModel.codebase.branches of
                -- This should never be the case
                Nothing ->
                    Cmd.none

                Just branch ->
                    Task.map2
                        Tuple.pair
                        (getMissingTermTypes newModel branch)
                        (getMissingTypeDecls newModel branch)
                        |> Task.attempt Http_GetTermTypesAndTypeDecls
    in
    ( newModel
    , command
    )


update_Http_GetPatches :
    Result (Http.Error Bytes) (List ( PatchHash, Patch ))
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetPatches result model =
    case result of
        Err err ->
            ( { model
                | errors = Err_GetPatches err :: model.errors
              }
            , Cmd.none
            )

        Ok result2 ->
            update_Http_GetPatches2 result2 model


update_Http_GetPatches2 :
    List ( PatchHash, Patch )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetPatches2 patches model =
    let
        updateCodebase :
            ModelCodebase
            -> ModelCodebase
        updateCodebase codebase =
            let
                newPatches : HashDict PatchHash Patch
                newPatches =
                    List.foldl
                        (\( hash, patch ) ->
                            HashDict.insert hash patch
                        )
                        codebase.patches
                        patches
            in
            { codebase
                | patches = newPatches
            }

        newModel : Model
        newModel =
            { model
                | codebase = updateCodebase model.codebase
            }
    in
    ( newModel
    , Cmd.none
    )


update_Http_GetTerm :
    Result (Http.Error Bytes) ( Id, Http.Response (Term Symbol) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTerm result model =
    case result of
        Err err ->
            ( { model
                | errors = Err_GetTerm err :: model.errors
              }
            , Cmd.none
            )

        Ok response ->
            update_Http_GetTerm2 response model


update_Http_GetTerm2 :
    ( Id, Http.Response (Term Symbol) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTerm2 ( id, response ) model =
    let
        updateCodebase :
            ModelCodebase
            -> ModelCodebase
        updateCodebase codebase =
            let
                newTerms : HashDict Id (Term Symbol)
                newTerms =
                    HashDict.insert id response.body codebase.terms
            in
            { codebase
                | terms = newTerms
            }

        newModel : Model
        newModel =
            { model
                | codebase = updateCodebase model.codebase
            }
    in
    ( newModel
    , Cmd.none
    )


update_Http_GetTermTypesAndTypeDecls :
    Result (Http.Error Bytes) ( List ( Id, Type Symbol ), List ( Id, Declaration Symbol ) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTermTypesAndTypeDecls result model =
    case result of
        Err err ->
            ( { model
                | errors = Err_GetTypeDecls err :: model.errors
              }
            , Cmd.none
            )

        Ok response ->
            update_Http_GetTermTypesAndTypeDecls2 response model


update_Http_GetTermTypesAndTypeDecls2 :
    ( List ( Id, Type Symbol ), List ( Id, Declaration Symbol ) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTermTypesAndTypeDecls2 ( termTypes, types ) model =
    let
        updateCodebase :
            ModelCodebase
            -> ModelCodebase
        updateCodebase codebase =
            let
                newTermTypes : HashDict Id (Type Symbol)
                newTermTypes =
                    List.foldl
                        (\( id, type_ ) -> HashDict.insert id type_)
                        codebase.termTypes
                        termTypes

                newTypeDecls : HashDict Id (Declaration Symbol)
                newTypeDecls =
                    List.foldl
                        (\( id, declaration ) -> HashDict.insert id declaration)
                        codebase.typeDecls
                        types
            in
            { codebase
                | termTypes = newTermTypes
                , typeDecls = newTypeDecls
            }

        newModel : Model
        newModel =
            { model
                | codebase = updateCodebase model.codebase
            }
    in
    ( newModel
    , Cmd.none
    )


jumpToTop : Cmd Message
jumpToTop =
    Dom.setViewport 0.0 0.0
        |> Task.perform (\_ -> NoOp)


{-| Click a branch:

  - Fetch all of the new branch's types and terms.

-}
update_User_ClickBranch :
    List NameSegment
    -> Branch
    -> Model
    -> ( Model, Cmd Message )
update_User_ClickBranch path branch model =
    let
        updateUI :
            ModelUI
            -> ModelUI
        updateUI ui =
            { ui
                | branch = path
            }

        newModel : Model
        newModel =
            { model
                | ui = updateUI model.ui
            }

        command : Cmd Message
        command =
            Task.map2
                Tuple.pair
                (getMissingTermTypes model branch)
                (getMissingTypeDecls model branch)
                |> Task.attempt Http_GetTermTypesAndTypeDecls
    in
    ( newModel
    , Cmd.batch [ command, jumpToTop ]
    )


{-| Focus a branch:

  - We might've already fetched it (e.g. it's the child of a previous root). In
    this case we have no branches to fetch, but we do want to fetch all of the
    new branch's types and terms (currently: just types).

  - Otherwise, it's somewhere in our history. So fetch it and all of its
    children! When the request comes back, we'll switch focus.

-}
update_User_FocusBranch :
    BranchHash
    -> Model
    -> ( Model, Cmd Message )
update_User_FocusBranch hash model =
    case HashDict.get hash model.codebase.branches of
        Nothing ->
            let
                command : Cmd Message
                command =
                    getBranch
                        model.api.unison
                        model.codebase
                        hash
                        |> Task.attempt Http_GetBranch
            in
            ( model
            , command
            )

        Just branch ->
            let
                updateCodebase :
                    ModelCodebase
                    -> ModelCodebase
                updateCodebase codebase =
                    { codebase
                        | head = Just hash
                    }

                updateUI :
                    ModelUI
                    -> ModelUI
                updateUI ui =
                    { ui
                        | branch = []
                    }

                newModel : Model
                newModel =
                    { model
                        | codebase = updateCodebase model.codebase
                        , ui = updateUI model.ui
                    }

                command : Cmd Message
                command =
                    Task.map2
                        Tuple.pair
                        (getMissingTermTypes newModel branch)
                        (getMissingTypeDecls newModel branch)
                        |> Task.attempt Http_GetTermTypesAndTypeDecls
            in
            ( newModel
            , command
            )


update_User_GetPatches :
    BranchHash
    -> Model
    -> ( Model, Cmd Message )
update_User_GetPatches hash model =
    let
        command : Cmd Message
        command =
            case HashDict.get hash model.codebase.branches of
                -- Should never be the case
                Nothing ->
                    Cmd.none

                Just branch ->
                    getMissingPatches
                        model
                        branch
                        |> Task.attempt Http_GetPatches
    in
    ( model
    , command
    )


update_User_HoverTerm :
    Hover
    -> Model
    -> ( Model, Cmd message )
update_User_HoverTerm hover model =
    let
        updateUI :
            ModelUI
            -> ModelUI
        updateUI ui =
            { ui
                | hovered = Just hover
            }

        newModel : Model
        newModel =
            { model
                | ui = updateUI model.ui
            }
    in
    ( newModel
    , Cmd.none
    )


update_User_Unhover : Model -> ( Model, Cmd message )
update_User_Unhover model =
    let
        updateUI :
            ModelUI
            -> ModelUI
        updateUI ui =
            { ui
                | hovered = Nothing
            }

        newModel : Model
        newModel =
            { model
                | ui = updateUI model.ui
            }
    in
    ( newModel
    , Cmd.none
    )


{-| The user has adjusted the current search.
-}
update_User_Search :
    String
    -> Model
    -> ( Model, Cmd msg )
update_User_Search search model =
    let
        updateSearch :
            ModelUI
            -> ModelUI
        updateSearch ui =
            { ui
                | search = search
            }

        newModel : Model
        newModel =
            { model
                | ui = updateSearch model.ui
            }
    in
    ( newModel
    , Cmd.none
    )


update_User_ToggleTerm :
    Id
    -> Model
    -> ( Model, Cmd Message )
update_User_ToggleTerm id model =
    let
        command : Cmd Message
        command =
            case HashDict.get id model.codebase.terms of
                Nothing ->
                    model.api.unison.getTerm id
                        |> Task.attempt Http_GetTerm

                Just _ ->
                    Cmd.none

        updateUI :
            ModelUI
            -> ModelUI
        updateUI ui =
            let
                newTerms : HashDict Id Bool
                newTerms =
                    HashDict.update
                        id
                        (maybe True not >> Just)
                        ui.terms
            in
            { ui
                | terms = newTerms
            }

        newModel : Model
        newModel =
            { model
                | ui = updateUI model.ui
            }
    in
    ( newModel
    , command
    )


subscriptions :
    Model
    -> Sub Message
subscriptions _ =
    Sub.none
