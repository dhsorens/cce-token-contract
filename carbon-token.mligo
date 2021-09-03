// The main contract that controls the carbon economics project 

// NOTE : 
// * deploy fa2 this address in storage and then add to the storage here 

(* =============================================================================
 * Storage
 * ============================================================================= *)

type storage = {
    addr_fa2 : address ; 
}

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type touch_fa2 = address * nat * nat //(owner, id, quantity)
type update_fa2_addr = address 
type mint_data = address * nat * nat //(owner, id, quantity)

type entrypoint = 
| Touch of touch_fa2 
| UpdateAddress of address 


type result = (operation list) * storage

(* =============================================================================
 * Error codes
 * ============================================================================= *)

let error_INCORRECT_ENTRYPOINT_TYPE = 0n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let touch_fa2 (param : touch_fa2) (storage : storage) : result = 
    let (owner,id,qty) = param in 
    let addr_fa2 = storage.addr_fa2 in 
    
    let param_touch : mint_data list = [ (owner, id, qty); ] in 
    let entrypoint_touch = (
        match (Tezos.get_entrypoint_opt "mint" addr_fa2 : mint_data list contract option) with 
        | None -> (failwith error_INCORRECT_ENTRYPOINT_TYPE : mint_data list contract)
        | Some c -> c
    ) in 

    let op_touch = Tezos.transaction param_touch 0tez entrypoint_touch in
    ([op_touch], storage)


let update_fa2_addr (param : address) (storage : storage) : result = 
    let new_addr_fa2 = param in 
    let new_storage = {storage with addr_fa2 = new_addr_fa2 ;} in 
    (([] : operation list), storage)


(* =============================================================================
 * Main
 * ============================================================================= *)

let main ((entrypoint, storage) : entrypoint * storage) : result =
    match entrypoint with 
    | Touch param -> 
        touch_fa2 param storage
    | UpdateAddress param -> 
        update_fa2_addr param storage



