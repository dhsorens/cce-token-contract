// The main contract that controls the carbon economics project 

// TODO : Permissions for who can add projects -- do this with a "whitelist" datatype.
//        That gives them the info and they have a privelege to exercise it if they want to.

#include "deploy-carbon-fa2.mligo"

(* =============================================================================
 * Storage
 * ============================================================================= *)

type project_owner = address
type project = {
    addr_project : address;
}

type create_project = (nat * token_metadata) list // (id, metadata) list
type mint_tokens = (address * nat * nat) list // (owner * token_id * amt) list
type bury_carbon = (project * nat * nat) list // project, token_id, amt_to_burn

type storage = {
    admin : address ;
    projects : (project_owner, project) big_map ; // owner -> project
}

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| CreateProject of create_project 
| MintTokens of mint_tokens
| BuryCarbon of bury_carbon 

type result = (operation list) * storage

(* =============================================================================
 * Error codes
 * ============================================================================= *)

let error_PROJECT_NOT_FOUND = 0n
let error_PERMISSIONS_DENIED = 1n
let error_COULD_NOT_GET_ENTRYPOINT = 2n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

let param_to_burn (proj,token_id,amt_to_burn : project * nat * nat) : operation = 
    let owner = Tezos.source in 
    let addr_proj = proj.addr_project in 

    // get op data
    let txndata_burn : (fa2_owner * fa2_token_id * fa2_amt) list = 
        [ (owner, token_id, amt_to_burn) ; ]
    in
    let entrypoint_burn = (
        match (Tezos.get_entrypoint_opt "%burn" addr_proj : (fa2_owner * fa2_token_id * fa2_amt) list contract option) with 
        | None -> (failwith error_COULD_NOT_GET_ENTRYPOINT : (fa2_owner * fa2_token_id * fa2_amt) list contract)
        | Some c -> c
    ) in 
    
    // create the operation
    Tezos.transaction txndata_burn 0tez entrypoint_burn


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let create_project (param : create_project) (storage : storage) : result = 
    let owner = Tezos.source in 
    
    // construct the initial storage for your project's FA2 contract
    let ledger = (Big_map.empty : (fa2_owner * fa2_token_id , fa2_amt) big_map) in 
    let operators = (Big_map.empty : (fa2_owner * fa2_operator * fa2_token_id, unit) big_map) in  
    let metadata = 
        List.fold_left 
        (fun (acc, (id, meta) : ((fa2_token_id, token_metadata) big_map) * (nat * token_metadata) ) 
            -> Big_map.update id (Some meta : token_metadata option) acc ) // ensures no duplicate token ids 
        (Big_map. empty : (fa2_token_id, token_metadata) big_map)
        param
    in 

    // initiate an FA2 contract w/permissions given to project contract
    let fa2_init_storage : storage_fa2 = {
        carbon_contract = Tezos.self_address ;
        ledger = ledger ;
        operators = operators ;
        metadata = metadata ;
    } in 
    let (op_new_fa2,addr_new_fa2) = 
        deploy_carbon_fa2 (None : key_hash option) 0tez fa2_init_storage in

    // update the local storage
    let updated_storage = { storage with 
        projects = Big_map.update owner (Some { addr_project = addr_new_fa2; } : project option) storage.projects ;
    } in 

    // final state
    ([op_new_fa2], updated_storage)

let mint_tokens (param : mint_tokens) (storage : storage) : result = 
    // TODO : Minting permissions/caps in supply
    let proj_owner = Tezos.sender in 
    let addr_proj : address = (
        match (Big_map.find_opt proj_owner storage.projects) with
        | None -> (failwith error_PROJECT_NOT_FOUND : address)
        | Some p -> p.addr_project
    ) in 
    
    let txndata_mint = param in 
    let entrypoint_mint = (
        match (Tezos.get_entrypoint_opt "%mint" addr_proj : mint_tokens contract option) with 
        | None -> (failwith error_COULD_NOT_GET_ENTRYPOINT : mint_tokens contract)
        | Some c -> c
    ) in 
    let op_mint = Tezos.transaction txndata_mint 0tez entrypoint_mint in 

    ([ op_mint ;], storage)


let bury_carbon (param : bury_carbon) (storage : storage) : result = 
    let ops_burn = List.map param_to_burn param in 
    (ops_burn, storage)

(* =============================================================================
 * Main
 * ============================================================================= *)

let main (entrypoint, storage : entrypoint * storage) : result =
    match entrypoint with 
    | CreateProject param ->
        create_project param storage
    | MintTokens param ->
        mint_tokens param storage
    | BuryCarbon param ->
        bury_carbon param storage