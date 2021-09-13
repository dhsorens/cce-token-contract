// The main contract that controls the carbon economics project 

// TODO : Permissions for who can add projects -- do this with a "whitelist" datatype.
//        That gives them the info and they have a privelege to exercise it if they want to.

#include "carbon-fa2.mligo"
let  main_fa2 = main
type storage_fa2 = storage 
type entrypoint_fa2 = entrypoint 
type result_fa2 = result


(* =============================================================================
 * Storage
 * ============================================================================= *)

// TODO : what other project information will you need, e.g. for the AMM?
type project_owner = address
type project = {addr_project : address;}
type create_project = (nat * token_metadata) list // (id, metadata) list
type add_token = {project_addr : address; token_id : nat; token_metadata : token_metadata;}

type update_whitelist = unit
type carbon_to_life = project * nat * nat // project, token_id, amt_to_burn

type storage = {
    admin : address ;
    projects : (project_owner, project) big_map ; // owner -> project
    // whitelist : (address, create_project) big_map ; // whitelist to be able to create a new project
    // life_addr : address ; // address of the LIFE contract
}

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| CreateProject of create_project 
| AddToken of add_token 
| UpdateWhitelist of update_whitelist
| CarbonToLife of carbon_to_life 


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



(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let create_project (param : create_project) (storage : storage) : result = 
    // construct the initial storage for your project's FA2 contract
    let fa2_ledger = (Big_map.empty : (fa2_owner * fa2_token_id , fa2_amt) big_map) in 
    let operators = // the project owner is operator for all her tokens 
        List.fold_left 
        (fun (acc, (id,meta) :((fa2_operator * fa2_token_id, unit) big_map) * (nat * token_metadata) ) 
            -> Big_map.update (Tezos.sender, id) (Some () : unit option) acc)
        (Big_map.empty : (fa2_operator * fa2_token_id, unit) big_map)
        param
    in 
    let metadata = 
        List.fold_left 
        (fun (acc, (id, meta) : ((fa2_token_id, token_metadata) big_map) * (nat * token_metadata) ) 
            -> Big_map.update id (Some meta : token_metadata option) acc ) // ensures no duplicate token ids 
        (Big_map. empty : (fa2_token_id, token_metadata) big_map)
        param
    in 

    // initiate an FA2 contract w/permissions given to project contract
    // TODO : This needs fixing
    let fa2_init_storage : storage_fa2 = {
        carbon_contract = Tezos.self_address ;
        fa2_ledger = fa2_ledger ;
        operators = operators ;
        metadata = metadata ;
    } in
    let (op_new_fa2,addr_new_fa2) = 
        Tezos.create_contract
        (None : key_hash option)
        0tez 
        fa2_init_storage
    in

    // update the local storage
    let updated_storage = { storage with 
        projects = Big_map.update Tezos.sender (Some { addr_project = addr_fa2; } : project option) storage.projects ;
    } in 

    // final state
    ([op_new_fa2], updated_storage)


let add_token (param : add_token) (storage : storage) : result = 
    // get data for the txn 
    let txndata_addToken = (param.token_id, param.token_metadata) in     
    let addr_fa2 = (
        match Big_map.find_opt Tezos.sender storage.projects with 
        | None -> (failwith error_PROJECT_NOT_FOUND : address)
        | Some proj -> proj.addr_project
    ) in 
    let entrypoint_addToken = (
        match (Tezos.get_entrypoint_opt "%add_token" addr_fa2 : (nat * token_metadata) contract option) with 
        | None -> (failwith error_COULD_NOT_GET_ENTRYPOINT : (nat * token_metadata) contract)
        | Some e -> e
    ) in 

    // call add_token to the correct contract 
    let op_addToken = Tezos.transaction txndata_addToken 0tez entrypoint_addToken in 

    ([op_addToken], storage)

let update_whitelist (_param : update_whitelist) (storage : storage) : result = 
    if Tezos.sender <> storage.admin then 
        (failwith error_PERMISSIONS_DENIED : result) else 
    (([] : operation list), storage)

let carbon_to_life (param : carbon_to_life) (storage : storage) : result = 
    let (proj, token_id, amt_to_burn) = param in 
    let owner = Tezos.source in 
    
    // burn the fa2
    let addr_proj = proj.addr_project in 
    let txndata_burn : (fa2_owner * fa2_token_id * fa2_amt) list = 
        [ (owner, token_id, amt_to_burn) ; ]
    in
    let entrypoint_burn = (
        match (Tezos.get_entrypoint_opt "%burn" addr_proj : (fa2_owner * fa2_token_id * fa2_amt) list contract option) with 
        | None -> (failwith error_COULD_NOT_GET_ENTRYPOINT : (fa2_owner * fa2_token_id * fa2_amt) list contract)
        | Some c -> c
    ) in 
    let op_burn_fa2 = Tezos.transaction txndata_burn 0tez entrypoint_burn in 

    // mint the LIFE token
    (*
    let amt_to_mint = <calculate amt to mint based on exchange rate with TEZ>
    let txndata_mint : (fa2_owner * fa2_token_id * fa2_amt) list = 
        [ (owner, 0n, amt_to_mint) ; ] // the LIFE token is fungible
    in
    let entrypoint_mint : (fa2_owner * fa2_token_id * fa2_amt) list contract = (
        match (Tezos.get_entrypoint_opt "%mint" storage.life_addr : (fa2_owner * fa2_token_id * fa2_amt) list contract option) with 
        | None -> (failwith error_COULD_NOT_GET_ENTRYPOINT)
        | Some c -> c
    ) in 
    let op_mint_LIFE = Tezos.transaction txndata_mint 0tez entrypoint_mint in 
    *)

    ([ op_burn_fa2 ; (* op_mint_LIFE ; *) ], storage)

(* =============================================================================
 * Main
 * ============================================================================= *)

let main (entrypoint, storage : entrypoint * storage) : result =
    match entrypoint with 
    | CreateProject param ->
        create_project param storage
    | AddToken param ->
        add_token param storage 
    | UpdateWhitelist param -> 
        update_whitelist param storage