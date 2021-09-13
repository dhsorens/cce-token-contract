(* ============================================================================
 * SRC: FA2 Carbon Contract 
 * ============================================================================ *)

#include "carbon.mligo"
let  main_carbon = main
type storage_carbon = storage 
type entrypoint_carbon = entrypoint 
type result_carbon = result

#include "carbon-fa2.mligo"
let  main_fa2 = main
type storage_fa2 = storage 
type entrypoint_fa2 = entrypoint 
type result_fa2 = result

#include "carbon-amm.mligo"
let  main_amm = main
type storage_amm = storage 
type entrypoint_amm = entrypoint 
type result_amm = result

#include "carbon-life.mligo"
let  main_life = main
type storage_life = storage 
type entrypoint_life = entrypoint 
type result_life = result



(* ============================================================================
 * Some Proxy Contracts
 * ============================================================================ *)



(* ============================================================================
 * Generic Setup Function
 * ============================================================================ *)

// initiates an instance with alice, bob, and an operator
let init_contracts (alice_bal : nat) (bob_bal : nat) = 
    // generate some implicit addresses
    let reset_state_unit = Test.reset_state 4n ([] : nat list) in
    let (addr_alice, addr_bob, addr_operator, addr_dummy) = 
        (Test.nth_bootstrap_account 0, Test.nth_bootstrap_account 1, 
         Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in 

    // initiate contract; both alice and bob have 1000n tokens
    let init_fa2_storage = {
        fa2_ledger = ( Big_map.literal [ ((addr_alice, 0n), alice_bal) ; ((addr_bob, 0n), bob_bal) ; ] );
        operators  = ( Big_map.literal [ (addr_operator, 0n); ] ) ; 
        metadata   = ( Big_map.empty : (fa2_token_id, token_metadata) big_map ) ;
    } in
    let (typed_addr_fa2, pgm_fa2, size_fa2) = 
        Test.originate main_fa2 init_fa2_storage 0tez in
     (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2)

(* A test to make sure the setup results as expected *)
let test_verify_setup = 
    let alice_bal = 100n in 
    let bob_bal   = 100n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in

    // assert the storage (balances) are what they should be 
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
    (assert (alice_balance = alice_bal), assert (bob_balance = bob_bal))

(* ============================================================================
 * Test Transfer 
 * ============================================================================ *)

(* Test a simple transfer *)
(* TODO: Paramaterize this over 100ds of random values *)
let test_transfer = 
    // contract setup 
    let alice_bal = 1000n in 
    let bob_bal   = 1000n in 
    let transfer_amt = 500n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in
    
    // transfer 500n from alice to bob 
    let alice_source = Test.set_source addr_alice in 
    let transfer_entrypoint = 
        ((Test.to_entrypoint "transfer" typed_addr_fa2) : transfer contract) in 
    let txn : transfer = (addr_alice , [ (addr_bob, 0n, transfer_amt); ] ) in
    let transfer_alice_to_bob = 
        Test.transfer_to_contract_exn transfer_entrypoint txn 0tez in 

    // query balances 
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
    (assert (alice_balance = abs(alice_bal - transfer_amt)), assert (bob_balance = (bob_bal + transfer_amt)))


(* Make sure an empty list of transfers behaves as expected *)
let test_transfer_empty = 
    // contract setup 
    let alice_bal = 0n in 
    let bob_bal = 0n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in
    
    // call the transfer entrypoint with an empty list from alice to bob 
    let alice_source = Test.set_source addr_alice in 
    let transfer_entrypoint = 
        ((Test.to_entrypoint "transfer" typed_addr_fa2) : transfer contract) in 
    let txn : transfer = (addr_alice , ([] : (fa2_to * fa2_token_id * fa2_amt) list) ) in
    let transfer_alice_to_bob = 
        Test.transfer_to_contract_exn transfer_entrypoint txn 0tez in 

    // query balances 
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
    (assert (alice_balance = alice_bal), assert (bob_balance = bob_bal))


let test_transfer_mutation = ()



(* ============================================================================
 * Test Update_operators Entrypoint
 * ============================================================================ *)
(* Add and remove an operator *) 
let test_operator_add = 
    // contract setup 
    let alice_bal = 100n in 
    let bob_bal = 100n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in

    // add dummy_address as an operator 
    let operator_src = Test.set_source addr_operator in 
    let entrypoint_add_operator : update_operators contract = 
        Test.to_entrypoint "update_operators" typed_addr_fa2 in 
    let txndata_add : update_operators = Add_operator (addr_operator, addr_dummy, 0n) in 
    let txn_add_operator = Test.transfer_to_contract_exn entrypoint_add_operator txndata_add 0tez in

    // verify addr_dummy is now an operator of token with id = 0n
    let fa2_storage = Test.get_storage typed_addr_fa2 in 
    assert ((Big_map.find_opt addr_dummy fa2_storage.operators) = (Some 0n))

let test_operator_remove = 
    // contract setup 
    let alice_bal = 100n in 
    let bob_bal = 100n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in

    // add addr_dummy as an operator 
    let operator_src = Test.set_source addr_operator in 
    let entrypoint_add_operator : update_operators contract = 
        Test.to_entrypoint "update_operators" typed_addr_fa2 in 
    let txndata_add : update_operators = Add_operator (addr_operator, addr_dummy, 0n) in 
    let txn_add_operator = Test.transfer_to_contract_exn entrypoint_add_operator txndata_add 0tez in

    // addr_dummy will now remove addr_operator from operators list 
    let dummy_src = Test.set_source addr_dummy in 
    let entrypoint_remove_operator : update_operators contract = 
        Test.to_entrypoint "update_operators" typed_addr_fa2 in 
    let txndata_remove : update_operators = Remove_operator (addr_dummy, addr_operator, 0n) in 
    let txn_remove_operator = Test.transfer_to_contract_exn entrypoint_remove_operator txndata_remove 0tez in 

    // assert that addr_dummy is an operator 
    let fa2_storage = Test.get_storage typed_addr_fa2 in 
    (
        assert ((Big_map.find_opt addr_dummy fa2_storage.operators) = (Some 0n)),
        assert ((Big_map.find_opt addr_operator fa2_storage.operators) = (None : nat option))
    )

let test_operator_mutation = () 


(* ============================================================================
 * Test Balance_of Entrypoint 
 * ============================================================================ *)
let test_balance = () // This is already done implicitly above?



(* Make sure the empty list behaves as expected *)
let test_balance_empty = 
    let alice_bal = 100n in 
    let bob_bal   = 100n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in

    // assert the storage (balances) are what they should be 
    let (typed_addr_get_bal, pgm_get_bal, size_get_bal) = Test.originate get_bal 0n 0tez in 
    let entrypoint_balance_of : balance_of contract = (Test.to_entrypoint "balance_of" typed_addr_fa2) in
    let addr_get_bal : get_balance_entrypoint contract = (Test.to_contract typed_addr_get_bal) in
    // alice's balance
    let alice_bal_query : balance_of = (([] : (fa2_owner * fa2_token_id) list), addr_get_bal) in 
    let get_balance_alice = Test.transfer_to_contract_exn entrypoint_balance_of alice_bal_query 0tez in ()


let test_balance_mutation = () 


(* ============================================================================
 * Test Mint Entrypoint 
 * ============================================================================ *)
(* mint 100n tokens and give them to alice *) 
let test_mint = 
    let alice_bal = 0n in 
    let bob_bal   = 0n in 
    let amt_to_mint = 100n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in
     
    // mint 100n tokens (of id = 0n) for alice 
    let operator_src = Test.set_source addr_operator in 
    let entrypoint_mint : mint contract = Test.to_entrypoint "mint" typed_addr_fa2 in 
    let txndata_mint : mint = [(addr_alice, 0n, amt_to_mint) ; ] in
    let txn_mint_tokens = Test.transfer_to_contract_exn entrypoint_mint txndata_mint 0tez in
    // assert the storage (balances) are what they should be 
    let (typed_addr_get_bal, pgm_get_bal, size_get_bal) = Test.originate get_bal 0n 0tez in 
    let entrypoint_balance_of : balance_of contract = (Test.to_entrypoint "balance_of" typed_addr_fa2) in
    let addr_get_bal : get_balance_entrypoint contract = (Test.to_contract typed_addr_get_bal) in
    // alice's balance
    let alice_bal_query : balance_of = ([ (addr_alice, 0n); ], addr_get_bal) in 
    let get_balance_alice = Test.transfer_to_contract_exn entrypoint_balance_of alice_bal_query 0tez in
    let alice_balance = Test.get_storage typed_addr_get_bal in 
    assert (alice_balance = amt_to_mint)


(* Make sure the empty list behaves as expected *)
let test_mint_empty = 
    let alice_bal = 0n in 
    let bob_bal   = 0n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in

    // mint nothing (the empty list)
    let operator_src = Test.set_source addr_operator in 
    let entrypoint_mint : mint contract = Test.to_entrypoint "mint" typed_addr_fa2 in 
    let txndata_mint : mint = [] in
    let txn_mint_tokens = Test.transfer_to_contract_exn entrypoint_mint txndata_mint 0tez in
    ()

(* make sure fails if minter is not operator *) 
(* THIS TEST FAILS WHEN RUN (which means it succeeds)
let test_mint_non_operator =  
    let alice_bal = 0n in 
    let bob_bal   = 0n in 
    let amt_to_mint = 100n in 
    let (addr_alice, addr_bob, addr_operator, addr_dummy, typed_addr_fa2) = 
        init_contracts alice_bal bob_bal in

    // mint 100n tokens (of id = 0n) for alice 
    //let operator_src = Test.set_source addr_operator in 
    let entrypoint_mint : mint contract = Test.to_entrypoint "mint" typed_addr_fa2 in 
    let txndata_mint : mint = [(addr_alice, 0n, amt_to_mint) ; ] in
    let txn_mint_tokens = Test.transfer_to_contract_exn entrypoint_mint txndata_mint 0tez in

    // assert the storage (balances) are what they should be 
    let (typed_addr_get_bal, pgm_get_bal, size_get_bal) = Test.originate get_bal 0n 0tez in 
    let entrypoint_balance_of : balance_of contract = (Test.to_entrypoint "balance_of" typed_addr_fa2) in
    let addr_get_bal : get_balance_entrypoint contract = (Test.to_contract typed_addr_get_bal) in
    // alice's balance
    let alice_bal_query : balance_of = ([ (addr_alice, 0n); ], addr_get_bal) in 
    let get_balance_alice = Test.transfer_to_contract_exn entrypoint_balance_of alice_bal_query 0tez in
    let alice_balance = Test.get_storage typed_addr_get_bal in 
    assert (alice_balance = amt_to_mint)
*)

let test_mint_mutation = () 


(* ============================================================================
 * Test Burn Entrypoint 
 * ============================================================================ *)
let test_burn = ()


let test_burn_empty = () 
    // Make sure the empty list behaves as expected 



let test_burn_mutation = () 



(* ============================================================================
 * Test Metadata Entrypoint 
 * ============================================================================ *)
let test_metadata = () 


let test_metadata_empty = () 
    // Make sure the empty list behaves as expected 



let test_metadata_mutation = () 

