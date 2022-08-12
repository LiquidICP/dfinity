import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Int "mo:base/Int";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Error "mo:base/Error";
import Result "mo:base/Result";
import Debug "mo:base/Debug";
import Iter "mo:base/Iter";
import Time "mo:base/Time";
import SHA224 "./SHA224";
import Hash "mo:base/Hash";

import Token "canister:token";

 actor Bridge {
    
    private stable var isInit = false;
    private stable var owner : Principal = Principal.fromText("aaaaa-aa"); 
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa"); 
    private stable var txcounter : Nat = 0;

    private stable var requestBridgingToEndInfoList = List.nil<RequestBridgingToEndInfo>();

    type RequestBridgingToEndInfo = {
        id : Nat;
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
                    id = txcounter;
                    caller = msg.caller;
                    address = _address;
                    amount = _amount;
                };
                requestBridgingToEndInfoList := List.push(transactionInfo, requestBridgingToEndInfoList);
                txcounter += 1;
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

    public query func getRequestBridgingToEndList() : async [RequestBridgingToEndInfo] {
        return List.toArray(requestBridgingToEndInfoList);
    };

    public shared(msg) func deleteRequestBridgingToEndInfosFromList(
        _ids : [Nat]
        ) : async Bool {
        if (msg.caller != owner) { 
            if (msg.caller != bot_messenger) { 
                return false;
            };
        };

        for (id in _ids.vals()) {
            requestBridgingToEndInfoList := await searchingForSingleItemInList(id);
        };
        return true;
    };

    private func searchingForSingleItemInList(_id : Nat) : async List.List<RequestBridgingToEndInfo> {
        var list = List.nil<RequestBridgingToEndInfo>();
        for (_requestBridgingToEndInfo in List.toArray(requestBridgingToEndInfoList).vals()) {
            if (_requestBridgingToEndInfo.id != _id) {
                list := List.push<RequestBridgingToEndInfo>(_requestBridgingToEndInfo, list);
            };
        };
        return list;
    };

};
