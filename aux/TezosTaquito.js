/*************** 
 * Initial data
 ***************/ 

const prefNetwork = 'granadanet' ; 
const carbonContractAddress = 'KT1A9nbXJcQqbtNiBzhrwqmEccDXawJaRbNW' ;

// wallet integration setup
const options = { name : 'CarbonMarketplace', preferredNetwork: prefNetwork, };
const wallet = new BeaconWallet(options);

// ask the user for wallet permissions 
wallet 
    .requestPermissions({ network: { type: prefNetwork} })
    .then((_) => wallet.getPKH()) // gets the address
    .then((address) => println(`Your address: ${address}`))

// set the wallet you'll use for the rest of the time here
Tezos.setWalletProvider(wallet);


/*************** 
 * Start a Project
 ***************/ 
// this calls the CreateProject entrypoint of the carbon.mligo contract 
// * must be of the form (nat * token_metadata) list
// * token_metadata has the form (string, bytes) map
// * these correspond to (id,   metadata      )

//TODO: how to get these values?
const createProjectData = [ (0, "metadata_map"), (1, "metadata_map1") , ] // etc


Tezos.wallet 
    .at(carbonContractAddress)
    .then((contract) => contract.methods.createProject(createProjectData).send())
    .then((op) => {
        println(`Hash: ${op.opHash}`);
        return op.confirmation();
    })
    .then((result) => {
        console.log(result);
        if (result.completed) {
            println(`Transaction correctly processed!
            Block: ${result.block.header.level}
            Chain ID: ${result.block.chain_id}`);
        } else {
            println('An error has occurred');
        }
    })
    .catch((err) => console.log(err));


/*************** 
 * Mint tokens 
 ***************/ 
// this calls the Mint entrypoint of the carbon.mligo contract
// * must be of the form (address * nat * nat) list
// * these correspond to (owner,    id,   amt)

// TODO: how to get these values?
const owner = "tz1T1buQd895VYtg34W3swFaVpT6A4XpW5i7" ;
const token_id = 0 ;
const amt = 100 ;
const mintData = [ (owner, token_id, amt) ];

Tezos.wallet 
    .at(carbonContractAddress)
    .then((contract) => contract.methods.mint(mintData).send())
    .then((op) => {
        println(`Hash: ${op.opHash}`);
        return op.confirmation();
    })
    .then((result) => {
        console.log(result);
        if (result.completed) {
            println(`Transaction correctly processed!
            Block: ${result.block.header.level}
            Chain ID: ${result.block.chain_id}`);
        } else {
            println('An error has occurred');
        }
    })
    .catch((err) => console.log(err));


/*************** 
 * Transfer Tokens
 ***************/ 
// this calls the Transfer entrypoint of the generated carbon_fa2.mligo contract
// * must be of the form  fa2_from * ((fa2_to * fa2_token_id * fa2_amt) list)

// TODO : How to get these values?
const transferData = "..."

Tezos.wallet 
    .at(carbonContractAddress)
    .then((contract) => contract.methods.transfer(transferData).send())
    .then((op) => {
        println(`Hash: ${op.opHash}`);
        return op.confirmation();
    })
    .then((result) => {
        console.log(result);
        if (result.completed) {
            println(`Transaction correctly processed!
            Block: ${result.block.header.level}
            Chain ID: ${result.block.chain_id}`);
        } else {
            println('An error has occurred');
        }
    })
    .catch((err) => console.log(err));


    
/*************** 
 * Transfer Tokens
 ***************/ 
// this calls:
//    * the Add_operator    entrypoint of the generated carbon-fa2.mligo contract 
//    * the BuryCarbon      entrypoint of the           carbon.mligo     contract
//    * the Remove_operator entrypoint of the generated carbon-fa2.mligo contract 

// TODO : How to get this data?
const addOperatorData     = "..." ;
const buryCarbonData      = "..." ;
const removeOperatorsData = "..." ;


Tezos.wallet 
    .at(carbonContractAddress)
    .then((contract) => contract.methods.add_operator(addOperatorData).send())
    .then((op) => {
        println(`Hash: ${op.opHash}`);
        return op.confirmation();
    })
    .then((contract) => contract.methods.buryCarbon(buryCarbonData).send())
    .then((op) => {
        println(`Hash: ${op.opHash}`);
        return op.confirmation();
    })
    .then((contract) => contract.methods.remove_operator(removeOperatorsData).send())
    .then((op) => {
        println(`Hash: ${op.opHash}`);
        return op.confirmation();
    })
    .then((result) => {
        console.log(result);
        if (result.completed) {
            println(`Transaction correctly processed!
            Block: ${result.block.header.level}
            Chain ID: ${result.block.chain_id}`);
        } else {
            println('An error has occurred');
        }
    })
    .catch((err) => console.log(err));