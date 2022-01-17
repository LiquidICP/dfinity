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

    private func _chargeFee(_principalFrom: Principal, _fee: Nat) {
        if (fee > 0) {
            _transfer(_principalFrom, feeWallet, _fee);
        };
    };

    private func _transfer(from: Principal, to: Principal, value: Nat) {
        let from_balance = _balanceOf(from);
        let from_balance_new : Nat = from_balance - value;
        if (from_balance_new != 0) { balances.put(from, from_balance_new); }
        else { balances.delete(from); };

        let to_balance = _balanceOf(to);
        let to_balance_new : Nat = to_balance + value;
        if (to_balance_new != 0) { balances.put(to, to_balance_new); };
    };

    private func _balanceOf(who: Principal) : Nat {
        switch (balances.get(who)) {
            case (?balance) { return balance; };
            case (_) { return 0; };
        }
    };

    private func _allowance(owner: Principal, spender: Principal) : Nat {
        switch(allowances.get(owner)) {
            case (?allowance_owner) {
                switch(allowance_owner.get(spender)) {
                    case (?allowance) { return allowance; };
                    case (_) { return 0; };
                }
            };
            case (_) { return 0; };
        }
    };


    public shared(msg) func transfer(to: Principal, value: Nat) : async TxReceipt {
        if (_balanceOf(msg.caller) < value + fee) { 
            return #err(#InsufficientBalance); 
        };
        _chargeFee(msg.caller, fee);
        _transfer(msg.caller, to, value);
        return #ok(1);
    };

    
    public shared(msg) func transferFrom(from: Principal, to: Principal, value: Nat) : async TxReceipt {
        if (_balanceOf(from) < value + fee) { return #err(#InsufficientBalance); };
        let allowed : Nat = _allowance(from, msg.caller);
        if (allowed < value + fee) { return #err(#InsufficientAllowance); };
        _transfer(from, to, value);
        let allowed_new : Nat = allowed - value - fee;
        if (allowed_new != 0) {
            let allowance_from = Types.unwrap(allowances.get(from));
            allowance_from.put(msg.caller, allowed_new);
            allowances.put(from, allowance_from);
        } else {
            if (allowed != 0) {
                let allowance_from = Types.unwrap(allowances.get(from));
                allowance_from.delete(msg.caller);
                if (allowance_from.size() == 0) { allowances.delete(from); }
                else { allowances.put(from, allowance_from); };
            };
        };
        return #ok(1);
    };

    public shared(msg) func approve(spender: Principal, value: Nat) : async TxReceipt {
        if(_balanceOf(msg.caller) < fee) { return #err(#InsufficientBalance); };
        _chargeFee(msg.caller, fee);
        let v = value + fee;
        if (value == 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.delete(spender);
            if (allowance_caller.size() == 0) { allowances.delete(msg.caller); }
            else { allowances.put(msg.caller, allowance_caller); };
        } else if (value != 0 and Option.isNull(allowances.get(msg.caller))) {
            var temp = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
            temp.put(spender, v);
            allowances.put(msg.caller, temp);
        } else if (value != 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.put(spender, v);
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

    public shared(msg) func burn(_amount : Nat): async TxReceipt {
        let from_balance = _balanceOf(msg.caller);
        if(from_balance < _amount) {
            return #err(#InsufficientBalance);
        };
        totalSupply -= _amount;
        balances.put(msg.caller, from_balance - _amount);
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

    public query func allowance(owner : Principal, spender : Principal) : async Nat {
        return _allowance(owner, spender);
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

    public query func getHolders(start : Nat, limit : Nat) : async [(Principal, Nat)] {
        let temp =  Iter.toArray(balances.entries());
        func order (a: (Principal, Nat), b: (Principal, Nat)) : Order.Order {
            return Nat.compare(b.1, a.1);
        };
        let sorted = Array.sort(temp, order);
        let limit_: Nat = if(start + limit > temp.size()) {
            temp.size() - start
        } else {
            limit
        };
        let res = Array.init<(Principal, Nat)>(limit_, (owner, 0));
        for (i in Iter.range(0, limit_ - 1)) {
            res[i] := sorted[i+start];
        };
        return Array.freeze(res);
    };

    public shared(msg) func getWrapperToken(_amount : Nat, senderPrincipal : Principal) : async TxReceipt {
        Debug.print("msg.cender=  " # debug_show msg.caller);
        if (msg.caller != owner) { 
            return #err(#Unauthorized);
        } else {
            let feeAmount : Nat = await calcCommission(_amount, fee);
            Debug.print("feeAmount=  " # debug_show feeAmount);
            let amount1 : Nat = _amount * 10**decimals - feeAmount;
            Debug.print("amount=  " # debug_show amount1);
            let resMint = mint(senderPrincipal, amount1);
            await distributeTokens(_amount, feeAmount);

            return #ok(1);
        };
    };

    private func distributeTokens(_amount : Nat, _feeAmount : Nat) : async () {
        await trancferICP(_feeAmount, feeWallet);
        let seventyPercentOfAmount : Nat = await calcCommission(_amount, 700);
        await trancferICP(seventyPercentOfAmount, owner);
    };

    private func trancferICP(_amount : Nat, principal : Principal) : async () {
        let now : Int = Time.now();
        let res = await Ledger.transfer({
          memo = 0;
          from_subaccount = null;
          to = Account.accountIdentifier(principal, Account.defaultSubaccount());
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

    public func calcCommission(_amount : Nat, _fee : Nat) : async Nat {
        return _amount * 10**decimals * _fee / MAX_BP
    };
    //TODO: Для тестов
    public shared(msg) func m(_pri: Principal) : async Account.AccountIdentifier {
        Account.accountIdentifier(_pri, Account.defaultSubaccount())
    };
    
};