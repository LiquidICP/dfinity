import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Types "./types";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Int "mo:base/Int";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Result "mo:base/Result";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import Error "mo:base/Error";

import Account "./Account";

import Ledger "canister:ledger";

actor Token {
    type Operation = Types.Operation;
    
    type TxReceipt = Result.Result<Nat, {
        #TransactionSuccessful;
        #InsufficientBalance;
        #InsufficientAllowance;
        #Unauthorized;
    }>;

    private stable var owner : Principal = Principal.fromText("aaaaa-aa");
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa");
    private stable var feeWallet : Principal = Principal.fromText("aaaaa-aa");
    private stable var name : Text = "";
    private stable var decimals : Nat = 0;
    private stable var symbol : Text = "";
    private stable var totalSupply : Nat = 0;
    private stable var blackhole : Principal = Principal.fromText("aaaaa-aa");
    private stable var fee : Nat = 0;
    private stable var isInit = false;
    private stable let MAX_BP = 1000;
    private stable var balanceEntries : [(Principal, Nat)] = [];
    private stable var allowanceEntries : [(Principal, [(Principal, Nat)])] = [];
    private var balances = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
    private var allowances = HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>>(1, Principal.equal, Principal.hash);
    
 
    public shared(msg) func init(
        _name: Text, 
        _symbol: Text,
        _decimals: Nat, 
        _owner: Principal,
        _feeWallet: Principal,
        _bot_messenger: Principal,
        _fee: Nat
        ) {
            assert(isInit == false);
            owner := _owner;
            bot_messenger := _bot_messenger;
            feeWallet := owner;
            name := _name;
            decimals := _decimals;
            symbol  := _symbol;
            fee := _fee;
            isInit := true;
        };

    private func _chargeFee(_principalFrom : Principal, _fee : Nat) {
        if (fee > 0) {
            _transfer(_principalFrom, feeWallet, _fee);
        };
    };

    private func _transfer(from : Principal, to : Principal, value : Nat) {
        let from_balance = _balanceOf(from);
        let from_balance_new : Nat = from_balance - value;
        if (from_balance_new != 0) { balances.put(from, from_balance_new); }
        else { balances.delete(from); };

        let to_balance = _balanceOf(to);
        let to_balance_new : Nat = to_balance + value;
        if (to_balance_new != 0) { balances.put(to, to_balance_new); };
    };

    private func _balanceOf(_account : Principal) : Nat {
        switch (balances.get(_account)) {
            case (?balance) { return balance; };
            case (_) { return 0; };
        }
    };

    private func _allowance(_owner : Principal, _spender : Principal) : Nat {
        switch(allowances.get(_owner)) {
            case (?allowance_owner) {
                switch(allowance_owner.get(_spender)) {
                    case (?allowance) { return allowance; };
                    case (_) { return 0; };
                }
            };
            case (_) { return 0; };
        }
    };


    public shared(msg) func transfer(_to : Principal, _amount : Nat) : async TxReceipt {
        if (_balanceOf(msg.caller) < _amount + fee) { 
            return #err(#InsufficientBalance); 
        };
        _chargeFee(msg.caller, fee);
        _transfer(msg.caller, _to, _amount);
        return #ok(1);
    };

    
    public shared(msg) func transferFrom(_from : Principal, _to : Principal, _amount: Nat) : async TxReceipt {
        if (_balanceOf(from) < _amount + fee) { return #err(#InsufficientBalance); };
        let allowed : Nat = _allowance(_from, msg.caller);
        if (allowed < _value + fee) { return #err(#InsufficientAllowance); };
        _transfer(_from, _to, _amount);
        let allowed_new : Nat = allowed - _value - fee;
        if (allowed_new != 0) {
            let allowance_from = Types.unwrap(allowances.get(_from));
            allowance_from.put(msg.caller, allowed_new);
            allowances.put(_from, allowance_from);
        } else {
            if (allowed != 0) {
                let allowance_from = Types.unwrap(allowances.get(_from));
                allowance_from.delete(msg.caller);
                if (allowance_from.size() == 0) { allowances.delete(_from); }
                else { allowances.put(_from, allowance_from); };
            };
        };
        return #ok(1);
    };

    public shared(msg) func approve(_spender : Principal, _amount : Nat) : async TxReceipt {
        if(_balanceOf(msg.caller) < fee) { return #err(#InsufficientBalance); };
        _chargeFee(msg.caller, fee);
        let v = _amount + fee;
        if (_amount == 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.delete(_spender);
            if (allowance_caller.size() == 0) { allowances.delete(msg.caller); }
            else { allowances.put(msg.caller, allowance_caller); };
        } else if (_amount != 0 and Option.isNull(allowances.get(msg.caller))) {
            var temp = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
            temp.put(_spender, v);
            allowances.put(msg.caller, temp);
        } else if (_amount != 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.put(_spender, v);
            allowances.put(msg.caller, allowance_caller);
        };
        return #ok(1);
    };

    public shared(msg) func mint(_principalTo: Principal, _amount : Nat) : async TxReceipt {
        if(msg.caller != owner or msg.caller != bot_messenger) {
            return #err(#Unauthorized);
        };
        let to_balance = _balanceOf(_principalTo);
        totalSupply += _amount;
        balances.put(_principalTo, to_balance + _amount);
        #ok(1);
    };

    public shared(msg) func burn(_account : Principal, _amount : Nat): async TxReceipt {
        let from_balance = _balanceOf(_account);
        if(from_balance < _amount) {
            return #err(#InsufficientBalance);
        };
        totalSupply -= _amount;
        balances.put(_account, from_balance - _amount);
        return #ok(1);
    };

    public query func getName() : async Text {
        return name;
    };

    public query func getSymbol() : async Text {
        return symbol;
    };

    public query func getDecimals() : async Nat {
        return decimals;
    };

    public query func getTotalSupply() : async Nat {
        return totalSupply;
    };

    public query func getTokenFee() : async Nat {
        return fee;
    };

    public query func balanceOf(_principal : Principal) : async Nat {
        return _balanceOf(_principal);
    };

    public query func allowance(_owner : Principal, _spender : Principal) : async Nat {
        return _allowance(_owner, _spender);
    };

    public shared(msg) func setFeeWallet(_fee : Principal) {
        assert(msg.caller == owner);
        feeWallet := _fee;
    };

    public shared(msg) func setFee(_fee : Nat) {
        assert(msg.caller == owner);
        fee := _fee;
    };

    public shared(msg) func setOwner(_owner : Principal) {
        assert(msg.caller == owner);
        owner := _owner;
    };

    public shared(msg) func getWrapperToken(_amount : Nat, _senderPrincipal : Principal) : async TxReceipt {
        Debug.print("msg.cender=  " # debug_show msg.caller);
        if (msg.caller != owner or msg.caller != bot_messenger) { 
            return #err(#Unauthorized);
        } else {
            let feeAmount : Nat = await calcCommission(_amount, fee);
            Debug.print("feeAmount=  " # debug_show feeAmount);
            let amount : Nat = _amount - feeAmount;
            Debug.print("amount=  " # debug_show amount);
            let resMint = mint(_senderPrincipal, amount);
            await distributeTokens(amount, _feeAmount)
            return #ok(1);
        };
    };

    private shared(msg) func distributeTokens(_amount : Nat, _feeAmount : Nat) : async TxReceipt {
        if (msg.caller != owner or msg.caller != bot_messenger) { 
            return #err(#Unauthorized);
        } else {
            await trancferICP(_feeAmount, feeWallet);
            let seventyPercentOfAmount : Nat = await calcCommission(_amount, 700);
            await trancferICP(seventyPercentOfAmount, owner);
        }
    };

    private func trancferICP(_amount : Nat, _account : Principal) : async () {
        let now : Int = Time.now();
        let res = await Ledger.transfer({
          memo = 0;
          from_subaccount = null;
          to = Account.accountIdentifier(_account, Account.defaultSubaccount());
          amount = { e8s = Nat64.fromNat(_amount) };
          fee = { e8s = 0 };
          created_at_time = ?{ timestamp_nanos = Nat64.fromNat(Int.abs(now)) };
        });
        switch (res) {
          case (#Ok(blockIndex)) {
            Debug.print("Paid reward to " # debug_show principal # " in block " # debug_show blockIndex);
          };
          case (#Err(#InsufficientFunds { balance })) {
            throw Error.reject("The balance is only " # debug_show balance # " e8s");
          };
          case (#Err(other)) {
            throw Error.reject("Unexpected error: " # debug_show other);
          };
        };
    };

    public shared(msg) func getICPFromWToken(_amount : Nat, _account : Principal) : async TxReceipt {
        if (msg.caller != owner or msg.caller != bot_messenger) { 
            return #err(#Unauthorized);
        } else {
            await trancferICP(_amount, _account);
            await burn(_account, _amount);
            return #ok(1);
        };
    };

    public func calcCommission(_amount : Nat, _fee : Nat) : async Nat {
        return _amount * _fee / MAX_BP
    };
    //TODO: Для тестов
    public shared(msg) func m(_pri: Principal) : async Account.AccountIdentifier {
        Account.accountIdentifier(_pri, Account.defaultSubaccount())
    };
    
};