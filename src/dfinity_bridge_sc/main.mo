import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Error "mo:base/Error";
import Result "mo:base/Result";

//import Ledger "canister:ledger";

import Token "canister:token";

 actor Bridge {
    
    private stable var fee : Nat = 0; //комиссия
    private stable var isInit = false;
    private stable var owner : Principal = Principal.fromText("aaaaa-aa"); // owner

    type TxReceipt = Result.Result<Nat, {
        #TransactionOk;
        #InsufficientBalance;
        #InsufficientAllowance;
        #Unauthorized;
    }>;

    public func init (_owner : Principal, _fee : Nat) {
        assert(isInit == false);
        owner := _owner;
        fee := _fee;
        isInit := true;
    };


    public shared(msg) func requestBridgingToEnd(amount : Nat) : async TxReceipt {
        await Token.transferFrom(msg.caller, Principal.fromActor(Bridge), amount);
    };
   
    public shared(msg) func performBridgingToStart(
        _amount : Nat,
        _principalTo : Principal
        ) : async TxReceipt {
            await Token.transferFrom(Principal.fromActor(Bridge), _principalTo, _amount);
    };


    public shared(msg) func greet() : async Principal {
        return msg.caller;
    };

    public func canisterTokenBalance() : async Nat {
        await Token.balanceOf(Principal.fromActor(Bridge));
    };

    public shared(msg) evacuateTokens(
        _canisterTokenPrincipal : Principal,
        _principalTo : Principal, 
        _amount : Nat
        ) async () {
            assert(msg.caller == owner);
            await Token.transferFrom(Principal.fromActor(Bridge), _principalTo, _amount);
        };

    public shared(msg) func setOwner(_owner : Principal) : async Bool{
        assert (msg.caller == owner);
        owner := _owner;
        return true;
    };

    public shared(msg) func setFee(_fee: Nat) {
        assert (true == (msg.caller == owner));
        fee := _fee;
    };

    public query func getFee() : async Nat {
        return fee;
    };

    public query func getOwner() : async Principal {
        return owner;
    };
};
