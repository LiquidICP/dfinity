import Principal "mo:base/Principal";
import Array "mo:base/Array";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Error "mo:base/Error";
import Result "mo:base/Result";
import Debug "mo:base/Debug";

import Token "canister:token";

 actor Bridge {
    
    private stable var isInit = false;
    private stable var owner : Principal = Principal.fromText("aaaaa-aa"); 
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa"); 
    private var initRequestBridgingToEndInfo : RequestBridgingToEndInfo = {
        caller = Principal.fromText("aaaaa-aa");
        address = "";
        amount = 0;
    };
    //private stable var requestBridgingToEndInfos : [var RequestBridgingToEndInfo] = Array.init<(RequestBridgingToEndInfo)>(1, initRequestBridgingToEndInfo);

    private stable var requestBridgingToEndList = List.nil<RequestBridgingToEndInfo>();


    type RequestBridgingToEndInfo = {
        caller : Principal;
        address : Text;
        amount : Nat;
    };

    public type TxReceipt = {
        #Ok : Nat;
        #Err : {
            #InsufficientAllowance;
            #InsufficientBalance;
            #ErrorOperationStyle;
            #Unauthorized;
            #LedgerTrap;
            #ErrorTo;
            #Other: Text;
            #BlockUsed;
            #AmountTooSmall;
        };
    };

    public func init(_owner : Principal, _bot_messenger : Principal) {
        assert(isInit == false);
        owner := _owner;
        bot_messenger := _bot_messenger;
        isInit := true;
    };

    public shared(msg) func requestBridgingToEnd(_amount : Nat, _address : Text) : async TxReceipt {
        let txReceipt : TxReceipt = await Token.transferFrom(msg.caller, Principal.fromActor(Bridge), _amount);
        switch (txReceipt){
            case (#Ok(blockIndex)) {
                let transactionInfo : RequestBridgingToEndInfo = {
                    caller = msg.caller;
                    address = _address;
                    amount = _amount;
                };
                requestBridgingToEndList := List.push(transactionInfo, requestBridgingToEndList);
                return #Ok(blockIndex);
            };
            case (#Err(#InsufficientBalance)) {
                return txReceipt;
            };
            case (_) {
                return #Err(#ErrorTo);
            }
        };
        return #Err(#ErrorTo);
    };
   
    public shared(msg) func performBridgingToStart(
        _amount : Nat,
        _account : Principal
        ) : async TxReceipt {
        if (msg.caller != owner) { 
            if (msg.caller != bot_messenger) { 
                return #Err(#Unauthorized);
            };
        };
        return await Token.transfer(_account, _amount);
    };

    public shared(msg) func canisterTokenBalance() : async Nat {
        await Token.balanceOf(Principal.fromActor(Bridge));
    };

    public shared(msg) func evacuateTokens(
        _principalTo : Principal, 
        _amount : Nat
        ) : async TxReceipt {
        if (msg.caller != owner) { 
            return #Err(#Unauthorized);
        } else {
           return await Token.transferFrom(Principal.fromActor(Bridge), _principalTo, _amount);
        };
    };

    public shared(msg) func setOwner(_owner : Principal) : async Bool {
        if (msg.caller != owner) { 
            return false;
        } else {
            owner := _owner;
            return true;
        };
    };

    public shared(msg) func setBotMassenger(_bot_messenger : Principal) : async Bool { 
        if (msg.caller != owner) { 
            return false;
        } else {
            bot_messenger := _bot_messenger;
            return true;
        };
    };

    public query func getOwner() : async Principal {
        return owner;
    };

    public query func getBotMassenger() : async Principal {
        return bot_messenger;
    };

    public query func getRequestBridgingToEndInfos() : async [RequestBridgingToEndInfo] {
        return List.toArray(requestBridgingToEndList);
    };

};
