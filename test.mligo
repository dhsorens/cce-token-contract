(* ============================================================================
 * SRC: FA2 Carbon Contract 
 * ============================================================================ *)

#include "carbon-token-fa2.mligo"
let  main = main
type storage = storage 
type entrypoint = entrypoint 
type result = result 


(* ============================================================================
 * Some Proxy Contracts
 * ============================================================================ *)

// Get Balance Bot 
type get_balance_storage    = nat 
type get_balance_entrypoint = (fa2_owner * fa2_token_id * fa2_amt) list
type get_balance_result     = (operation list) * get_balance_storage
let get_bal (param, storage : get_balance_entrypoint * get_balance_storage) : get_balance_result = 
    match param with 
    | [] -> (([] : operation list ), storage)
    | txn :: txns -> 
        (let (ownr, id, amt) = txn in (([] : operation list ), amt))

(* ============================================================================
 * Test Transfer 
 * ============================================================================ *)

// initiates an instance with alice, bob, and an operator
let test_setup = 
    // generate some implicit addresses
    let reset_state_unit = Test.reset_state 3n ([] : nat list) in
    let (addr_alice, addr_bob, addr_operator) = 
        (Test.nth_bootstrap_account 0, Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2) in 

    // initiate contract; both alice and bob have 1000n tokens
    let init_storage = {
        fa2_ledger = ( Big_map.literal [ ((addr_alice, 0n), 100n) ; ((addr_bob, 0n), 1000n) ; ] );
        operators  = ( Big_map.literal [ (addr_operator, 0n); ] ) ; 
        metadata   = ( Big_map.empty : (fa2_token_id, token_metadata) big_map ) ;
    } in
    let (typed_addr_fa2, pgm_fa2, size_fa2) = 
        Test.originate main init_storage 0tez in

    // assert the storage is what it should be 
    Test.michelson_equal (Test.get_storage_of_address (Tezos.address (Test.to_contract typed_addr_fa2))) (Test.compile_value init_storage)
    


let test_transfer = 
    // generate some implicit addresses
    let reset_state_unit = Test.reset_state 3n ([] : nat list) in
    let (addr_alice, addr_bob, addr_operator) = 
        (Test.nth_bootstrap_account 0, Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2) in 

    // initiate contract; both alice and bob have 1000n tokens
    let init_storage = {
        fa2_ledger = ( Big_map.literal [ ((addr_alice, 0n), 1000n) ; ((addr_bob, 0n), 1000n) ; ] );
        operators  = ( Big_map.literal [ (addr_operator, 0n); ] ) ; 
        metadata   = ( Big_map.empty : (fa2_token_id, token_metadata) big_map ) ;
    } in
    let (typed_addr_fa2, pgm_fa2, size_fa2) = 
        Test.originate main init_storage 0tez in

    // transfer 500n from alice to bob 
    let alice_source = Test.set_source addr_alice in 
    let transfer_entrypoint = 
        ((Test.to_entrypoint "transfer" typed_addr_fa2) : transfer contract) in 
    let txn : transfer = (addr_alice , [ (addr_bob, 0n, 500n); ] ) in
    let transfer_alice_to_bob = 
        Test.transfer_to_contract_exn transfer_entrypoint txn 0tez in 

    let (typed_addr_get_bal, pgm_get_bal, size_get_bal) = Test.originate get_bal 0n 0tez in 
    let entrypoint_balance_of : balance_of contract = (Test.to_entrypoint "balance_of" typed_addr_fa2) in
    let addr_get_bal : get_balance_entrypoint contract = (Test.to_contract typed_addr_get_bal) in
    // alice's balance
    let alice_bal_query : balance_of = ([ (addr_alice, 0n); ], addr_get_bal) in 
    let get_balance_alice = Test.transfer_to_contract_exn entrypoint_balance_of alice_bal_query 0tez in
    let alice_balance = Test.get_storage typed_addr_get_bal in 
    // bob's balance
    let bob_bal_query : balance_of = ([ (addr_bob, 0n); ], addr_get_bal) in 
    let get_balance_bob = Test.transfer_to_contract_exn entrypoint_balance_of bob_bal_query 0tez in
    let bob_balance = Test.get_storage typed_addr_get_bal in 
    
    // test that alice_balance = 500n and bob_balance = 1500n 
    (assert (alice_balance = 500n), assert (bob_balance = 1500n))



let test_transfer_empty = true 
    // Make sure an empty list of transfers behaves as expected


let test_transfer_mutation = true 



(* ============================================================================
 * Test Update_operators Entrypoint
 * ============================================================================ *)
let test_operator = true
    // Add and remove an operator -- 


let test_operator_mutation = true 


(* ============================================================================
 * Test Balance_of Entrypoint 
 * ============================================================================ *)
let test_balance = true



let test_balance_empty = true 
    // Make sure the empty list behaves as expected 



let test_transfer_mutation = true 


(* ============================================================================
 * Test Mint Entrypoint 
 * ============================================================================ *)
let test_mint = true 


let test_mint_empty = true 
    // Make sure the empty list behaves as expected 


let test_mint_mutation = true 


(* ============================================================================
 * Test Burn Entrypoint 
 * ============================================================================ *)
let test_burn = true


let test_burn_empty = true 
    // Make sure the empty list behaves as expected 



let test_burn_mutation = true 



(* ============================================================================
 * Test Metadata Entrypoint 
 * ============================================================================ *)
let test_metadata = true 


let test_metadata_empty = true 
    // Make sure the empty list behaves as expected 



let test_metadata_mutation = true 

