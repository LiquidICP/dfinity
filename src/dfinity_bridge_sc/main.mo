import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Error "mo:base/Error";
import Result "mo:base/Result";
import Debug "mo:base/Debug";

import Token "canister:token";

 actor Bridge {
    
    private stable var fee : Nat = 0; 
    private stable var isInit = false;
    private stable var owner : Principal = Principal.fromText("aaaaa-aa"); 
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa"); 

    type TxReceipt = Result.Result<Nat, {
        #TransactionSuccessful;
        #InsufficientBalance;
        #InsufficientAllowance;
        #Unauthorized;
    }>;

    public func init(_owner : Principal, _fee : Nat, _bot_messenger : Principal) {
        assert(isInit == false);
        owner := _owner;
        bot_messenger := _bot_messenger;
        fee := _fee;
        isInit := true;
    };


    public shared(msg) func requestBridgingToEnd(amount : Nat) : async TxReceipt {
        if (msg.caller != owner or msg.caller != bot_messenger) { 
            return #err(#Unauthorized);
        } else {
            await Token.transferFrom(msg.caller, Principal.fromActor(Bridge), amount);
        }
    };
   
    public shared(msg) func performBridgingToStart(
        _amount : Nat,
        _principalTo : Principal
        ) : async TxReceipt {
        if (msg.caller != owner or msg.caller != bot_messenger) { 
            return #err(#Unauthorized);
        } else {
            await Token.transferFrom(Principal.fromActor(Bridge), _principalTo, _amount);
        }
    };

    public func canisterTokenBalance() : async Nat {
        await Token.balanceOf(Principal.fromActor(Bridge));
    };

    public shared(msg) func evacuateTokens(
        _canisterTokenPrincipal : Principal,
        _principalTo : Principal, 
        _amount : Nat
        ) : async TxReceipt {
        if (msg.caller != owner) { 
            return #err(#Unauthorized);
        } else {
            await Token.transferFrom(Principal.fromActor(Bridge), _principalTo, _amount);
            return #ok(1);
        };
    };

    public shared(msg) func setOwner(_owner : Principal) : async TxReceipt{
        if (msg.caller != owner) { 
            return #err(#Unauthorized);
        } else {
            owner := _owner;
            return #ok(1);
        };
    };

    public shared(msg) func setBotMassenger(_bot_messenger : Principal) : async TxReceipt{
        if (msg.caller != owner) { 
            return #err(#Unauthorized);
        } else {
            _bot_messenger := _bot_messenger;
            return #ok(1);
        };
    };

    public shared(msg) func setFee(_fee: Nat) : async TxReceipt{
        if (msg.caller != owner) { 
            return #err(#Unauthorized);
        } else {
            fee := _fee;
            return #ok(1);
        };
    };

    public query func getFee() : async Nat {
        return fee;
    };

    public query func getOwner() : async Principal {
        return owner;
    };
};
