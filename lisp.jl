abstract type Sexp
end

struct Symbol <: Sexp
    sym_string::String
end

Base.show(io::IO, sym::Symbol) = print(io, sym.sym_string)

symbol_table = Dict{String, Symbol}();

function symbol(str::String)    
    key = uppercase(str)
    if (haskey(symbol_table, key))
        return symbol_table[key]
    else
        symbol_table[key] = Symbol(key)
        return symbol_table[key]
    end
end

built_in = Dict{Symbol, Any}()
built_in[symbol("T")] = symbol("T")
built_in[symbol("NIL")] = symbol("NIL")
built_in[symbol("CAR")] = symbol("CAR")
built_in[symbol("CDR")] = symbol("CDR")
built_in[symbol("CONS")] = symbol("CONS")
built_in[symbol("IF")] = symbol("IF")
built_in[symbol("DEF")] = symbol("DEF")
built_in[symbol("SET")] = symbol("SET")
built_in[symbol("QUOTE")] = symbol("QUOTE")
built_in[symbol("EVAL")] = symbol("EVAL")
built_in[symbol("LAMBDA")] = symbol("LAMBDA")
built_in[symbol("ATOM")] = symbol("ATOM")

global_var = Dict{Symbol, Any}()

global_var_list = [built_in, global_var]
lexical_var_list = []

function save_env()
    tmp = []
    for vars in lexical_var_list
        push!(tmp, vars)
    end
    return tmp
end

function load_env(env)
    size = length(lexical_var_list)
    for i in 1:size
        pop!(lexical_var_list)
    end
    for vars in env
        push!(lexical_var_list, vars)
    end
end

function get_value(sym::Symbol)
    for i in length(lexical_var_list):-1:1
        if haskey(lexical_var_list[i], sym)
            return lexical_var_list[i][sym]
        end
    end
    for i in length(global_var_list):-1:1
        if haskey(global_var_list[i], sym)
            return global_var_list[i][sym]
        end
    end
end

function def(symbol, obj)
    if (!haskey(built_in, symbol) && typeof(symbol) == Symbol)
        global_var[symbol] = obj
        return obj
    else
        # error
    end
end

function set(sym, obj)
    if !haskey(built_in, sym) && typeof(sym) == Symbol
        for i in length(lexical_var_list):-1:1
            if haskey(lexical_var_list[i], sym)
                lexical_var_list[i][sym] = obj
                return obj
            end
        end
        for i in length(global_var_list):-1:1
            if haskey(global_var_list[i], sym)
                global_var_list[i][sym] = obj
                return obj
            end
        end
    else
        # error
    end
end

mutable struct Cell <: Sexp
    car
    cdr
end

Base.show(io::IO, cell::Cell) = print(io, "(", cell.car, " . ", cell.cdr, ")")

function atom(obj)
    if typeof(obj) <: Sexp && typeof(obj) != Cell 
        return symbol("T")
    else
        return symbol("NIL")
    end
end

function cons(obj1, obj2)
    return Cell(obj1, obj2)
end

function car(sexp)
    if sexp == symbol("NIL")
        return symbol("NIL")
    else
        return sexp.car
    end
end

function cdr(sexp)
    if sexp == symbol("NIL")
        return symbol("NIL")
    else
        return sexp.cdr
    end
end

function eq(obj1, obj2)
    if obj1 == obj2
        return symbol("T")
    else
        return symbol("NIL")
    end
end

mutable struct Lambda <: Sexp
    env
    args
    body
end


function lambda(args, body)
    env = []
    for vars in lexical_var_list
        push!(env, vars)
    end
    return Lambda(env, args, body)
end

function eval_symbol(sexp)
    return get_value(sexp)
end

function eval_sexp(sexp)
    if atom(sexp) == symbol("T")
        return eval_symbol(sexp)
    else
        first_elt = eval_sexp(car(sexp))
        if first_elt == symbol("car")
            return car(eval_sexp(car(cdr(sexp))))
        elseif first_elt == symbol("cdr")
            return cdr(eval_sexp(car(cdr(sexp))))
        elseif first_elt == symbol("cons")
            return cons(eval_sexp(car(cdr(sexp))), eval_sexp(car(cdr(cdr(sexp)))))
        elseif first_elt == symbol("quote")
            return car(cdr(sexp))
        elseif first_elt == symbol("atom")
            return atom(car(cdr(sexp)))
        elseif first_elt == symbol("if")
            if eval_sexp(car(cdr(sexp))) == symbol("NIL")
                return eval_sexp(car(cdr(cdr(cdr(sexp)))))
            else
                return eval_sexp(car(cdr(cdr(sexp))))
            end
        elseif first_elt == symbol("eval")
            return eval_sexp(car(cdr(sexp)))
        elseif first_elt == symbol("def")
            return def(car(cdr(sexp)), eval_sexp(car(cdr(cdr(sexp)))))
        elseif first_elt == symbol("set")
            return set(car(cdr(sexp)), eval_sexp(car(cdr(cdr(sexp)))))
        elseif first_elt == symbol("lambda")
            return lambda((car(cdr(sexp))), (cdr(cdr(sexp))))
        elseif typeof(first_elt) == Lambda
            tmp = save_env()
            load_env(first_elt.env)
            args = Dict()
            arg = car(first_elt.args)
            rest_args = cdr(first_elt.args)
            val = car(cdr(sexp))
            rest_vals = cdr(cdr(sexp))
            while arg != symbol("NIL")
                args[arg] = eval_sexp(val)
                arg = car(cdr(rest_args))
                rest_args = cdr(cdr(rest_args))
                val = car(cdr(rest_vals))
                rest_vals = cdr(cdr(rest_vals))
            end
            push!(lexical_var_list, args)
            result = symbol("NIL")
            elt = car(first_elt.body)
            rest_body = cdr(first_elt.body)
            while elt != symbol("NIL")
                result = eval_sexp(elt)
                elt = car(cdr(rest_body))
                rest_body = cdr(cdr(rest_body))                
            end
            load_env(tmp)
            return result
        end
    end
end

function is_open_paren(char::Char)
    return '(' == char
end

function is_close_paren(char::Char)
    return ')' == char
end

function is_number_char(char::Char)
    return 48 <= Int(char) <= 57
end

function is_upper_ascii_alphabet(char::Char)
    return 65 <= Int(char) <= 90
end

function is_lower_ascii_alphabet(char::Char)
    return 97 <= Int(char) <= 122
end

function is_other_symbol_char(char::Char)
    if 33 == Int(char)
        return true
    elseif 42 <= Int(char) < 45
        return true
    elseif 47 == Int(char)
        return true
    elseif 60 <= Int(char) <= 64
        return true
    else
        return false
    end
end

function is_symbol_char(char::Char)
    if is_number_char(char)
        return true
    elseif is_upper_ascii_alphabet(char)
        return true
    elseif is_lower_ascii_alphabet(char)
        return true
    elseif is_other_symbol_char(char)
        return true
    else
        return false
    end
end

function is_space_char(char::Char)
    return 32 == Int(char)
end

function is_dot_char(char::Char)
    return 46 == Int(char)
end


function skip_space(str, pos)
    if is_space_char(str[pos])
        pos +=1
    else
        return pos
    end
end

function read_symbol_(str, pos, stack)
    if length(str) < pos
        return (symbol(uppercase(join(stack))), pos)
    elseif is_symbol_char(str[pos])
        push!(stack, str[pos])
        pos += 1
        return read_symbol_(str, pos, stack)
    else
        return (symbol(uppercase(join(stack))), pos)
    end
end

function read_symbol(str, pos)
    return read_symbol_(str, pos, [])
end 

const out_of_list = 0
const top_of_list = 1
const rest_of_list = 2
const cdr_of_cell = 3

let
    pos = 1
    str = ""
    function initialize(position, input_string)
        pos = position
        str = input_string
    end
    function read_string_(state)
        if is_space_char(str[pos])
            pos = skip_space(str, pos)
            read_string_(state)
        elseif is_symbol_char(str[pos])
            if state == out_of_list || state == top_of_list || state == cdr_of_cell
                sym, pos = read_symbol(str, pos)
                return sym
            elseif state == rest_of_list
                sym, pos = read_symbol(str, pos)
                return cons(sym, read_string_(rest_of_list))
            end
        elseif is_open_paren(str[pos])
            pos += 1
            if is_close_paren(str[pos])
                if state == out_of_list || state == top_of_list || state == cdr_of_cell
                    pos += 1
                    return symbol("NIL")
                elseif state == rest_of_list
                    pos += 1
                    return cons(symbol("NIL"),
                                read_string_(rest_of_list))
                end
            elseif is_space_char(str[pos])
                pos = skip_space(str, pos)
            else
                return cons(read_string_(state),
                            read_string_(rest_of_list))
            end
        elseif is_close_paren(str[pos])
            if state == out_of_list
                # TODO error
            else state == rest_of_list || state == cdr_of_cell
                pos += 1
                return symbol("NIL")
            end
        elseif is_dot_char(str[pos])
            if state == out_of_list || state == top_of_list || state == cdr_of_cell
                # TODO error
            elseif state == rest_of_list
                pos += 1
                read_string_(cdr_of_cell)
            end
        end
    end
    global function read_string(str)
        initialize(1, str)
        return read_string_(out_of_list)
    end
end

function fake_repl()
    value = 0
    while value != read_string("EXIT")
        print("LISP?> ")
        value = eval_sexp(read_string(readline()))
        println(value)
    end
end

fake_repl()
