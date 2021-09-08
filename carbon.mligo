// The main contract that controls the carbon economics project 

// TODO : Permissions for who can add projects -- do this with a "greenlit" datatype.
//        That gives them the info and they have a privelege to exercise it if they want to.

#include "carbon-project.mligo"
let  main_project = main
type storage_project = storage 
type entrypoint_project = entrypoint 
type result_project = result

#include "carbon-token-fa2.mligo"
let  main_fa2 = main
type storage_fa2 = storage 
type entrypoint_fa2 = entrypoint 
type result_fa2 = result


(* =============================================================================
 * Storage
 * ============================================================================= *)

// TODO : what other project information will you need, e.g. for the AMM?
type project = {addr_project : address;}
type create_project = {owner : address; metadata : metadata list;}
type add_token = {project_addr : address; token_id : address; token_metadata : metadata;}
type update_whitelist = ()

type storage = {
    admin : address ;
    projects : (address, project) big_map ; // owner -> project
    // whitelist : (address, create_project) big_map ; // whitelist to be able to create a new project
}

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| CreateProject of create_project 
| AddToken of add_token 
| UpdateWhitelist of update_whitelist

type result = (operation list) * storage

(* =============================================================================
 * Error codes
 * ============================================================================= *)

let error_PROJECT_NOT_FOUND = 0n
let error_PERMISSIONS_DENIED = 1n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)



(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let create_project (create_project : create_project) (storage : storage) : result = 
    // construct the initial storage for your project's FA2 contract
    let fa2_ledger = (Big_map.empty : (fa2_owner * fa2_token_id , fa2_amt) big_map) in 
    let operators = // the project owner is operator for all her tokens 
        List.fold_left 
        (fun (acc, (id,meta) :((fa2_operator * fa2_token_id, unit) big_map) * (nat * token_metadata) ) 
            -> Big_map.update (Tezos.sender, token_id) (Some () : unit option) acc)
        (Big_map.empty : (fa2_operator * fa2_token_id, unit) big_map)
        create_project
    in 
    let metadata = 
        List.fold_left 
        (fun (acc, (id, meta) : ((fa2_token_id, token_metadata) big_map) * (nat * token_metadata) ) 
            -> Big_map.update id (Some meta : token_metadata option) acc ) // ensures no duplicate token ids 
        (Big_map. empty : (fa2_token_id, token_metadata) big_map)
        create_project
    in 

    // initiate an FA2 contract w/permissions given to project contract
    let fa2_init_storage : storage_fa2 = {
        fa2_ledger = fa2_ledger ;
        operators = operators ;
        metadata = metadata ;
    }
    let (op_new_fa2,addr_new_fa2) = 
        Tezos.create_contract
            main_fa2
            (None : key_hash option) // the baker
            0tez
            fa2_init_storage
    in

    // update the local storage
    let updated_storage = { storage with 
        projects = Big_map.update Tezos.sender (Some { addr_project = addr_fa2; } : project option) storage.projects ;
    }

    // final state
    ([op_new_fa2], updated_storage)


let add_token (param : add_token) (storage : storage) : result = 
    // get data for the txn 
    let txndata_addToken = (param.token_id, param.token_metadata) in     
    let addr_fa2 = (
        match Big_map.find_opt Tezos.sender storage.projects with 
        | None -> (failwith error_PROJECT_NOT_FOUND : address)
        | Some addr -> addr
    ) in 
    let entrypoint_addToken = (
        match Tezos.get_entrypoint_opt "add_token" addr_fa2 with 
        | None -> (failwith error_PROJECT_NOT_FOUND : (nat * metadata) contract option)
        | Some e -> e
    ) in 

    // call add_token to the correct contract 
    let op_addToken = Tezos.transaction txndata_addToken 0tez entrypoint_addToken in 

    ([op_addToken], storage)

let update_whitelist (param : update_whitelist) (storage : storage) : result = 
    if Tezos.sender <> storage.admin then 
        (failwith error_PERMISSIONS_DENIED : result) else 
    (([] : operation list), storage)

(* =============================================================================
 * Main
 * ============================================================================= *)

let main ((entrypoint, storage) : entrypoint * storage) : result =
    match entrypoint with 
    | CreateProject param ->
        create_project param 
    | AddToken param ->
        add_token param