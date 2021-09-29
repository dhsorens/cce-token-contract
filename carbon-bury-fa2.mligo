// An FA2 contract that manages the LIFE token
// Minting can only be done by the carbon contract
// The exchange rate is dependent on the AMM exchange rate

// These will be "negative-carbon tez", so when you burn them you're 
// trading them for the equivalent amount in tez.
// -- what if they are stably tied to tez ... ? Or ctez? 
//    The exchange rate could change in response to the target
//    All would be equal except for a scalar which responds to the target
//    and keeps it on track.