const repl = require("node:repl");

/**
 * @template V,E
 */
class Result {
  constructor() {}
  /**
   *
   * @param {V} value
   * @returns {Result.<V, unknown>}
   */
  static ok(value) {
    const self = new Result();

    self.is_ok = true;
    self.value = value;
    self.error = undefined;

    // console.info(`[ok] ${JSON.stringify(value)}`);

    return self;
  }

  /**
   *
   * @param {E} error
   * @returns {Result.<unknown, E>}
   */
  static err(error) {
    const self = new Result();

    self.is_ok = false;
    self.value = undefined;
    self.error = error;

    // console.error(`[err] ${error}`);

    return self;
  }

  /**
   *
   * @returns {boolean}
   */
  isok() {
    return this.is_ok;
  }

  /**
   *
   * @param {function(V): W} f
   * @returns {Result.<W, E>}
   * @template W
   */
  map(f) {
    return this.bind((v) => Result.ok(f(v)));
  }

  /**
   *
   * @param {function(V): W} fv
   * @param {function(E): H} fe
   * @returns {Result.<W, H>}
   * @template W,H
   */
  bi_bind(fv, fe) {
    return this.is_ok ? fv(this.value) : fe(this.error);
  }

  /**
   *
   * @param {function(V): Result<W,H>} f
   * @returns {Result.<W, H>}
   * @template W,H
   */
  bind(f) {
    return this.is_ok ? f(this.value) : this;
  }

  /**
   *
   * @param {function(): Result<V,E>} f
   * @returns {Result.<V, E>}
   */
  or(f) {
    return this.is_ok ? this : f();
  }

  static all(...results) {
    return results.reduce(
      (u = Result.ok([]), r = Result.err(undefined)) =>
        r.bind((v) => u.map((rs) => [...rs, v])),
      Result.ok([])
    );
  }
}

/**
 * @template T
 */
class Stack {
  constructor() {
    this.stack = [];
  }

  /**
   *
   * @param {T} v
   * @returns {Stack.<T>}
   */
  push(v) {
    this.stack.push(v);

    return this;
  }

  /**
   * @returns {Stack.<T>}
   */
  pop() {
    this.stack.pop();

    return this;
  }

  /**
   *
   * @returns {Result.<T, string>}
   */
  peek() {
    return this.stack.length === 0
      ? Result.err("empty stack")
      : Result.ok(this.stack[this.stack.length - 1]);
  }

  /**
   *
   * @param {function(U, T): U} f
   * @param {U} u
   * @returns {U}
   * @template U
   */
  fold(f, u0) {
    let u = u0;
    for (let i = this.stack.length - 1; i >= 0; i--) {
      u = f(u, this.stack[i]);
    }

    return u;
  }
}

/**
 * @template T
 */
class Frame {
  /**
   *
   * @param {string} id
   * @param {T} value
   */
  constructor(id, value) {
    this.id = id;
    this.value = value;
  }

  /**
   *
   * @param {string} name
   * @returns boolean
   */
  is(name) {
    return name === this.id;
  }

  /**
   *
   * @returns {T}
   */
  get() {
    return this.value;
  }
}

/*--------------------------------:
    Tokenization
:--------------------------------*/

const tokenize = (src = "") => {
  let current = "";
  let tokens = [];

  const push = (...args) => {
    if (current) {
      tokens.push(current);
      current = "";
    }
    args.forEach((a) => tokens.push(a));
  };

  for (const s of src.split("")) {
    if (/\s/.test(s)) {
      push();
    } else if ("()λ.".includes(s)) {
      push(s);
    } else {
      current += s;
    }
  }

  push();
  return tokens;
};

/*--------------------------------:
    AST
:--------------------------------*/

const AST = {
  LAMBDA: "lambda",
  VAR: "var",
  APPLICATION: "application",
  NIL: "nil",
  BUILTIN: "built in",
};

const lambda = (arg, body) => ({
  type: AST.LAMBDA,
  arg,
  body,
});

const variable = (id) => ({
  type: AST.VAR,
  id,
});

const application = (operator, operand) => ({
  type: AST.APPLICATION,
  operator,
  operand,
});

const nil = () => ({
  type: AST.NIL,
});

const eq = (a, b) => {
  if (a.type !== b.type) return false;

  switch (a.type) {
    case AST.VAR:
      return a.id === b.id;
    case AST.APPLICATION:
      return eq(a.operator, b.operator) && eq(a.operand, b.operand);
    case AST.LAMBDA:
      return eq(a.arg, b.arg) && eq(a.body, b.body);
    case AST.NIL:
      return true;
  }
};

/*--------------------------------:
    Parser
:--------------------------------*/
const parser = (tokens = ["(", ")"]) => {
  /**
   *
   * @param {number} position
   * @returns {Result}
   */
  const get_token = (position = 0) =>
    position < tokens.length
      ? Result.ok(tokens[position])
      : Result.err(`Tokens out of range: ${position}/${tokens.length - 1}`);

  function parse_application(position = 0) {
    return parse(position).bind(([operator, next_position]) =>
      parse(next_position).map(([operand, next_position]) => [
        application(operator, operand),
        next_position,
      ])
    );
  }

  function parse_lambda(position = 0) {
    return parse_token("λ", position)
      .bind(([_, next_position]) => parse_var(next_position))
      .bind(([arg, next_position]) =>
        parse_token(".", next_position)
          .bind(([_, next_position]) => parse(next_position))
          .map(([body, next_position]) => [lambda(arg, body), next_position])
      );
  }

  function parse_var(position = 0) {
    const forbidden = ["(", ")", ".", "λ"];

    return get_token(position).bind((token) => {
      if (forbidden.includes(token)) {
        return Result.err(
          `Variable name cannot include "${forbidden}". Found "${token}"`
        );
      }

      return Result.ok([variable(token), position + 1]);
    });
  }

  function parse_token(token = "", position = 0) {
    return get_token(position).bind((t) =>
      t === token
        ? Result.ok([token, position + 1])
        : Result.err(`parse_token: expected "${token}" got "${t}"`)
    );
  }

  function in_parentheses(parser) {
    return (position) =>
      parse_token("(", position)
        .bind(([_, next_position]) => parser(next_position))
        .bind(([result, next_position]) =>
          parse_token(")", next_position).map(([_, next_position]) => [
            result,
            next_position,
          ])
        );
  }

  function parse(position = 0) {
    return parse_var(position)
      .or(() => parse_lambda(position))
      .or(() => in_parentheses(parse_lambda)(position))
      .or(() => in_parentheses(parse_application)(position));
  }

  return parse(0);
};

/*--------------------------------:
    Evaluator
:--------------------------------*/

/**
 *
 * @param {Stack.<Frame.<any>>} stack0
 * @returns
 */
const evaluator = (stack0) => (ast) => {
  /**
   * @type {Stack.<Frame.<any>>}
   */
  const stack = stack0;

  const lookup = (id) => {
    const not_found = Result.err(
      `variable ${id} is undefined in the current scope`
    );

    return stack.fold(
      (u, frame) =>
        u.isok() ? u : frame.is(id) ? Result.ok(frame.get()) : not_found,
      not_found
    );
  };

  const replace_free = (id) => {
    const replace = (node) => {
      switch (node.type) {
        case AST.VAR:
          return node.id === id ? lookup(id) : Result.ok(node);
        case AST.APPLICATION:
          return replace(node.operator).bind((operator) =>
            replace(node.operand).map((operand) =>
              application(operator, operand)
            )
          );
        case AST.LAMBDA:
          return node.arg.id === id
            ? Result.ok(node)
            : replace(node.body).map((body) => lambda(node.arg, body));
        case AST.NIL:
          return Result.ok(node);
      }
    };

    return replace;
  };

  /**
   *
   * @param {*} node
   * @returns {Result.<object, string>}
   */
  const eval = (node) => {
    switch (node.type) {
      case AST.VAR:
        return lookup(node.id).or(() => Result.ok(node));
      case AST.APPLICATION:
        return eval(node.operator).bind((operator) => {
          switch (operator.type) {
            case AST.LAMBDA:
              return eval(node.operand).bind((operand) => {
                stack.push(new Frame(operator.arg.id, operand));

                return replace_free(operator.arg.id)(operator.body)
                  .bind(eval)
                  .map((r) => (stack.pop(), r));
              });

            case AST.VAR:
              return eval(node.operand).map((r) => application(operator, r));
            case AST.BUILTIN:
              return eval(node.operand).bind((operand) =>
                operator.f(stack, operand)
              );
            default:
              return Result.ok(application(operator, node.operand));
          }
        });
      case AST.LAMBDA: {
        let res = node.body;

        while (true) {
          console.dir(res, {depth: 10})
          let new_res = eval(res).or(() => Result.ok(res)).value;

          if (eq(res, new_res)) break;
          res = new_res;
        }

        return Result.ok(lambda(node.arg, res));
      }

      default:
        return Result.err(`unknown expression: ${JSON.stringify(node)}`);
    }
  };

  return eval(ast);
};

/*--------------------------------:
    Printer
:--------------------------------*/
const print = (node) => {
  switch (node.type) {
    case AST.APPLICATION:
      return `(${print(node.operator)} ${print(node.operand)})`;
    case AST.LAMBDA:
      return `(λ ${print(node.arg)}. ${print(node.body)})`;
    case AST.VAR:
      return node.id;
    case AST.NIL:
      return "()";
  }
};

const builtin = (id, f) => ({
  type: AST.BUILTIN,
  id,
  f,
});

const make_stdlib = (...builtins) => {
  const stack = new Stack();
  builtins.forEach((b) => stack.push(new Frame(b.id, b)));

  return stack;
};

const stdlib = make_stdlib(
  builtin(
    "+",
    (() => {
      const num = (x) => Number(x.id);

      return (_, a) =>
        !Number.isNaN(num(a))
          ? Result.ok(
              builtin("$1+", (_, b) =>
                !Number.isNaN(num(b))
                  ? Result.ok(variable(`${num(a) + num(b)}`))
                  : Result.err(`cannot add NaN ${JSON.stringify(b)}`)
              )
            )
          : Result.err(`cannot add NaN ${JSON.stringify(a)}`);
    })()
  )
);

/*--------------------------------:
    REPL
:--------------------------------*/
repl.start({
  prompt: "> ",
  eval(cmd, _ctx, _file, cb) {
    cb(
      null,
      cmd
        ? parser(tokenize(cmd))
            .bind(([ast, _]) => evaluator(stdlib)(ast))
            .bi_bind(
              (ast) => Result.ok(print(ast)),
              (error) => Result.ok(error)
            ).value
        : ""
    );
  },
});

module.exports = {
  tokenize,
  Result,
};
