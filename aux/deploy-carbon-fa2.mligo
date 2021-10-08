#include "carbon-fa2-types.mligo"
type storage_fa2 = storage

let deploy_carbon_fa2 (delegate : key_hash option) (amnt : tez) (init_storage : storage_fa2) = 
    Tezos.create_contract
    (fun (entrypoint, storage : entrypoint * storage) : result ->
        let error_FA2_TOKEN_UNDEFINED = 0n in 
        let error_FA2_INSUFFICIENT_BALANCE = 1n in
        let error_FA2_NOT_OPERATOR = 4n in
        let error_PERMISSIONS_DENIED = 10n in
        let rec main (entrypoint, storage : entrypoint * storage) : result = (
            match entrypoint with
            | Transfer param -> (
                match param.to with
                | [] -> (([] : operation list), storage)
                | hd :: tl ->
                    let (from, to, token_id, qty) = (param.from, hd.to, hd.token_id, hd.qty) in 
                    // check permissions
                    let operator = Tezos.sender in 
                    let owner = from in 
                    let not_operator : bool = 
                        match Big_map.find_opt (owner, operator, token_id) storage.operators with 
                        | None -> true
                        | Some () -> false in 
                    if ((Tezos.sender <> from) && not_operator) then (failwith error_FA2_NOT_OPERATOR : result) else 
                    // check balance
                    let sender_token_balance =
                        match Big_map.find_opt (from, token_id) storage.ledger with
                        | None -> 0n
                        | Some token_balance -> token_balance in
                    let recipient_balance = 
                        match Big_map.find_opt (to, token_id) storage.ledger with
                        | None -> 0n
                        | Some recipient_token_balance -> recipient_token_balance in
                    if (sender_token_balance < qty) then (failwith error_FA2_INSUFFICIENT_BALANCE : result) else
                    // update the ledger
                    let ledger = 
                        Big_map.update
                        (to, token_id)
                        (Some (recipient_balance + qty))
                            (Big_map.update 
                            (from, token_id) 
                            (Some (abs (sender_token_balance - qty))) 
                            storage.ledger) in 
                    let storage = {storage with ledger = ledger ; } in
                    let param = { from = from ; to = tl ; } in 
                    main (Transfer(param), storage))
            | Balance_of param -> (
                let (request_list, callback) = (param.owner_data, param.callback) in 
                let accumulator = ([] : token_ownership list) in
                let rec owner_and_id_to_balance (param : (token_ownership list) * (owner_data list) * ((fa2_owner * fa2_token_id , fa2_amt) big_map)) : token_ownership list = 
                    let (accumulator, request_list, ledger) = param in
                    match request_list with
                    | [] -> accumulator 
                    | h :: t -> 
                        let owner = h.owner in 
                        let token_id = h.token_id in
                        let qty =
                            match Big_map.find_opt (owner, token_id) ledger with 
                            | None -> 0n
                            | Some owner_balance -> owner_balance in
                        let accumulator = { owner = owner ; token_id = token_id ; qty = qty ; } :: accumulator in
                        owner_and_id_to_balance (accumulator, t, ledger) in
                let ack_list = owner_and_id_to_balance (accumulator, request_list, storage.ledger) in
                let t = Tezos.transaction ack_list 0mutez callback in
                ([t], storage))
            | Update_operators param -> (
                match param with
                | Add_operator o ->
                    let (token_owner, operator, token_id) = (o.token_owner, o.operator, o.token_id) in 
                    // check permissions        
                    if (Tezos.source <> token_owner) then (failwith error_PERMISSIONS_DENIED : result) else
                    // update storage
                    let storage = {storage with 
                        operators = Big_map.update (token_owner, operator, token_id) (Some ()) storage.operators ; } in 
                    (([] : operation list), storage)
                | Remove_operator o ->
                    let (token_owner, operator, token_id) = (o.token_owner, o.operator, o.token_id) in 
                    // check permissions
                    if (Tezos.sender <> token_owner) then (failwith error_PERMISSIONS_DENIED : result) else
                    // update storage
                    let storage = {storage with 
                        operators = Big_map.update (token_owner,operator,token_id) (None : unit option) storage.operators ; } in 
                    (([] : operation list), storage))
            | Mint param -> (
                let minting_list = param in
                match minting_list with 
                | [] -> (([] : operation list), storage)
                | hd :: tl -> 
                    let owner = hd.owner in 
                    let token_id = hd.token_id in 
                    let qty = hd.qty in 
                    // check operator
                    if Tezos.sender <> storage.carbon_contract then (failwith error_FA2_NOT_OPERATOR : result) else 
                    // update owner balance
                    let owner_balance = 
                        match Big_map.find_opt (owner, token_id) storage.ledger with
                        | None -> 0n + qty
                        | Some ownerPrevBalance -> ownerPrevBalance + qty in
                    let storage = {storage with 
                        ledger = Big_map.update (owner, token_id) (Some owner_balance) storage.ledger ; } in 
                    main (Mint(tl), storage))
            | Burn param -> (
                // check permissions
                if Tezos.sender <> storage.carbon_contract then (failwith error_PERMISSIONS_DENIED : result) else 
                let burn_addr = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) in 
                let from = Tezos.source in 
                // transfer the tokens to the burn address
                let txndata_burn = {
                    from = from ;
                    to = List.map
                        (fun (b : token_ownership) : transfer_to -> 
                            let () = assert (b.owner = from) in 
                            {
                                to = burn_addr ;
                                token_id = b.token_id ;
                                qty = b.qty ;
                            })
                        param ;
                } in 
                main (Transfer(txndata_burn), storage))
            | Get_metadata param -> (
                let query_list = param.token_ids in 
                let callback = param.callback in 
                let metadata_list = 
                    List.map 
                    (fun (token_id : nat) : callback_metadata -> 
                        match Big_map.find_opt token_id storage.metadata with 
                        | None -> (failwith error_FA2_TOKEN_UNDEFINED : callback_metadata) 
                        | Some m -> {token_id = token_id ; token_metadata = m ; })
                    query_list in 
                let op_metadata = Tezos.transaction metadata_list 0tez callback in 
                ([op_metadata] , storage))) in
        main (entrypoint, storage))
        (* End of contract code for the project FA2 contract *)
    delegate
    amnt 
    init_storage
    

