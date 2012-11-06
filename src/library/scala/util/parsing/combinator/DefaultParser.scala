package scala.util.parsing.combinator
import scala.annotation.migration


import debugging.ParserLocation

trait DefaultParser {
  self: Parsers =>

  object AbsParserFactory extends ParserFactory {
    def apply[T](f: Input => ParseResult[T]): Parser[T] = new DefaultParser[T] {
      def consume(in: Input) = f(in)
    }
    def apply[T](f: Input => ParseResult[T], loc0: debugging.ParserLocation): Parser[T]
      = apply[T](f)
    def apply[T](f: Input => ParseResult[T], loc0: debugging.ParserLocation, ps0: => List[Parser[T]]): Parser[T]
      = apply[T](f)
  }

  val mkParser:ParserFactory = AbsParserFactory

  def phrase[T](p: Parser[T])(implicit loc: debugging.ParserLocation): Parser[T] = new DefaultParser[T] {
    def consume(in: Input) = lastNoSuccessVar.withValue(None) {
      p(in) match {
        case s @ Success(out, in1) =>
          if (in1.atEnd) s
          else           lastNoSuccessVar.value filterNot { _.next.pos < in1.pos } getOrElse Failure("end of input expected", in1)
        case ns =>
          lastNoSuccessVar.value.getOrElse(ns)
      }
    }
  }

  def OnceParser[T](f: Input => ParseResult[T])(implicit loc: debugging.ParserLocation): Parser[T] with OnceParser[T]
    = new DefaultParser[T] with OnceParser[T] { def consume(in: Input) = f(in) }

  /** The root class of parsers.
   *  Parsers are functions from the Input type to ParseResult.
   */
  abstract class DefaultParser[+T] extends AbsParser[T] {
    protected var name: String = ""
    def named(n: String): this.type = {name=n; this}
    override def toString() = "Parser ("+ name +")"

    /** An unspecified method that defines the behaviour of this parser. */
    def consume(in: Input): ParseResult[T]

    def apply(in: Input): ParseResult[T] = consume(in)

    def flatMap[U](f: T => Parser[U])(implicit loc: ParserLocation): Parser[U]
      = mkParser( {in => this(in) flatMapWithNext(f)}, loc).named("parser-flatmap-" + name)

    def map[U](f: T => U)(implicit loc: ParserLocation): Parser[U] //= flatMap{x => success(f(x))}
      = mkParser( {in => this(in) map(f)}, loc).named("parser-map-" + name)

    def filter(p: T => Boolean): Parser[T]
      = withFilter(p)

    def withFilter(p: T => Boolean): Parser[T]
      = mkParser{ in => this(in) filterWithError(p, "Input doesn't match filter: "+_, in)}

    // no filter yet, dealing with zero is tricky!

    // todo: should loc be implicit?
    @migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
    def append[U >: T](p0: => Parser[U])(loc: ParserLocation): Parser[U] = { lazy val p = p0 // lazy argument
      mkParser({ in => this(in) append p(in)}, loc, List(this, p))
    }

    // the operator formerly known as +++, ++, &, but now, behold the venerable ~
    // it's short, light (looks like whitespace), has few overloaded meaning (thanks to the recent change from ~ to unary_~)
    // and we love it! (or do we like `,` better?)

    /** A parser combinator for sequential composition.
     *
     * `p ~ q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
     *
     * @param q a parser that will be executed after `p` (this parser)
     *          succeeds -- evaluated at most once, and only when necessary.
     * @return a `Parser` that -- on success -- returns a `~` (like a `Pair`,
     *         but easier to pattern match on) that contains the result of `p` and
     *         that of `q`. The resulting parser fails if either `p` or `q` fails.
     */
    @migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
    def ~ [U](q: => Parser[U])(implicit loc: ParserLocation): Parser[~[T, U]] = { lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield new ~(a,b)).named("~")
    }

    /** A parser combinator for sequential composition which keeps only the right result.
     *
     * `p ~> q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
     *
     * @param q a parser that will be executed after `p` (this parser)
     *        succeeds -- evaluated at most once, and only when necessary.
     * @return a `Parser` that -- on success -- returns the result of `q`.
     */
    @migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
    def ~> [U](q: => Parser[U])(implicit loc: ParserLocation): Parser[U] = { lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield b).named("~>")
    }

    /** A parser combinator for sequential composition which keeps only the left result.
     *
     *  `p <~ q` succeeds if `p` succeeds and `q` succeeds on the input
     *           left over by `p`.
     *
     * @note <~ has lower operator precedence than ~ or ~>.
     *
     * @param q a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when necessary
     * @return a `Parser` that -- on success -- returns the result of `p`.
     */
    @migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
    def <~ [U](q: => Parser[U])(implicit loc: ParserLocation): Parser[T] = { lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield a).named("<~")
    }

     /* not really useful: V cannot be inferred because Parser is covariant in first type parameter (V is always trivially Nothing)
    def ~~ [U, V](q: => Parser[U])(implicit combine: (T, U) => V): Parser[V] = new Parser[V] {
      def apply(in: Input) = seq(Parser.this, q)((x, y) => combine(x,y))(in)
    }  */

    /** A parser combinator for non-back-tracking sequential composition.
     *
     *  `p ~! q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
     *   In case of failure, no back-tracking is performed (in an earlier parser produced by the `|` combinator).
     *
     * @param p a parser that will be executed after `p` (this parser) succeeds
     * @return a `Parser` that -- on success -- returns a `~` (like a Pair, but easier to pattern match on)
     *         that contains the result of `p` and that of `q`.
     *         The resulting parser fails if either `p` or `q` fails, this failure is fatal.
     */
    def ~! [U](p: => Parser[U])(implicit loc: ParserLocation): Parser[~[T, U]] = {
      OnceParser{ (for(a <- this; b <- commit(p)) yield new ~(a,b)).named("~!") }
    }
    

    /** A parser combinator for alternative composition.
     *
     *  `p | q` succeeds if `p` succeeds or `q` succeeds.
     *   Note that `q` is only tried if `p`s failure is non-fatal (i.e., back-tracking is allowed).
     *
     * @param q a parser that will be executed if `p` (this parser) fails (and allows back-tracking)
     * @return a `Parser` that returns the result of the first parser to succeed (out of `p` and `q`)
     *         The resulting parser succeeds if (and only if)
     *         - `p` succeeds, ''or''
     *         - if `p` fails allowing back-tracking and `q` succeeds.
     */
    def | [U >: T](q: => Parser[U])(implicit loc: ParserLocation): Parser[U] = append(q)(loc).named("|")

    /** A parser combinator for alternative with longest match composition.
     *
     *  `p ||| q` succeeds if `p` succeeds or `q` succeeds.
     *  If `p` and `q` both succeed, the parser that consumed the most characters accepts.
     *
     * @param q0 a parser that accepts if p consumes less characters. -- evaluated at most once, and only when necessary
     * @return a `Parser` that returns the result of the parser consuming the most characters (out of `p` and `q`).
     */
    @migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
    def ||| [U >: T](q0: => Parser[U])(implicit loc: ParserLocation): Parser[U] = new DefaultParser[U] {
      lazy val q = q0 // lazy argument
      def consume(in: Input) = {
        val res1 = DefaultParser.this(in)
        val res2 = q(in)

        (res1, res2) match {
          case (s1 @ Success(_, next1), s2 @ Success(_, next2)) => if (next2.pos < next1.pos) s1 else s2
          case (s1 @ Success(_, _), _) => s1
          case (_, s2 @ Success(_, _)) => s2
          case (e1 @ Error(_, _), _) => e1
          case (f1 @ Failure(_, next1), ns2 @ NoSuccess(_, next2)) => if (next2.pos < next1.pos) f1 else ns2
        }
      }
      override def toString = "|||"
      val location: ParserLocation = loc
        
    }

    /** A parser combinator for function application.
     *
     *  `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the result of `p`.
     *
     * @param f a function that will be applied to this parser's result (see `map` in `ParseResult`).
     * @return a parser that has the same behaviour as the current parser, but whose result is
     *         transformed by `f`.
     */
    def ^^ [U](f: T => U)(implicit loc0: ParserLocation): Parser[U] =
      map(f)(loc0).named(toString+"^^")
    
  //    def ^^[U](f: T => U): Parser[U] = ^^(f, NoParserLocation)

    /** A parser combinator that changes a successful result into the specified value.
     *
     *  `p ^^^ v` succeeds if `p` succeeds; discards its result, and returns `v` instead.
     *
     * @param v The new result for the parser, evaluated at most once (if `p` succeeds), not evaluated at all if `p` fails.
     * @return a parser that has the same behaviour as the current parser, but whose successful result is `v`
     */
    @migration("The call-by-name argument is evaluated at most once per constructed Parser object, instead of on every need that arises during parsing.", "2.9.0")
    def ^^^ [U](v: => U)(implicit loc0: ParserLocation): Parser[U] =  new DefaultParser[U] {
      val location: ParserLocation = loc0
      lazy val v0 = v // lazy argument
      def consume(in: Input) = DefaultParser.this(in) map (x => v0)
    }.named(toString+"^^^")

    /** A parser combinator for partial function application.
     *
     *  `p ^? (f, error)` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
     *  in that case, it returns `f` applied to the result of `p`. If `f` is not applicable,
     *  error(the result of `p`) should explain why.
     *
     * @param f a partial function that will be applied to this parser's result
     *          (see `mapPartial` in `ParseResult`).
     * @param error a function that takes the same argument as `f` and produces an error message
     *        to explain why `f` wasn't applicable
     * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
     *         to the result. If so, the result will be transformed by `f`.
     */
    def ^? [U](f: PartialFunction[T, U], error: T => String)(implicit loc: ParserLocation): Parser[U] = mkParser ({ in =>
      this(in).mapPartial(f, error)}, loc).named(toString+"^?")

    /** A parser combinator for partial function application.
     *
     *  `p ^? f` succeeds if `p` succeeds AND `f` is defined at the result of `p`;
     *  in that case, it returns `f` applied to the result of `p`.
     *
     * @param f a partial function that will be applied to this parser's result
     *          (see `mapPartial` in `ParseResult`).
     * @return a parser that succeeds if the current parser succeeds <i>and</i> `f` is applicable
     *         to the result. If so, the result will be transformed by `f`.
     */
    def ^? [U](f: PartialFunction[T, U])(implicit loc: ParserLocation): Parser[U] = ^?(f, r => "Constructor function not defined at "+r)

    /** A parser combinator that parameterizes a subsequent parser with the
     *  result of this one.
     *
     *  Use this combinator when a parser depends on the result of a previous
     *  parser. `p` should be a function that takes the result from the first
     *  parser and returns the second parser.
     *
     *  `p into fq` (with `fq` typically `{x => q}`) first applies `p`, and
     *  then, if `p` successfully returned result `r`, applies `fq(r)` to the
     *  rest of the input.
     *
     *  ''From: G. Hutton. Higher-order functions for parsing. J. Funct. Program., 2(3):323--343, 1992.''
     *
     *  @example {{{
     *  def perlRE = "m" ~> (".".r into (separator => """[^%s]*""".format(separator).r <~ separator))
     *  }}}
     *
     *  @param fq a function that, given the result from this parser, returns
     *         the second parser to be applied
     *  @return a parser that succeeds if this parser succeeds (with result `x`)
     *          and if then `fq(x)` succeeds
     */
    def into[U](fq: T => Parser[U])(implicit loc: ParserLocation): Parser[U] = {
        flatMap(fq)
      }

    /** Changes the failure message produced by a parser.
     *
     *  This doesn't change the behavior of a parser on neither
     *  success nor error, just on failure. The semantics are
     *  slightly different than those obtained by doing `| failure(msg)`,
     *  in that the message produced by this method will always
     *  replace the message produced, which is not guaranteed
     *  by that idiom.
     *
     *  For example, parser `p` below will always produce the
     *  designated failure message, while `q` will not produce
     *  it if `sign` is parsed but `number` is not.
     *
     *  {{{
     *  def p = sign.? ~ number withFailureMessage  "Number expected!"
     *  def q = sign.? ~ number | failure("Number expected!")
     *  }}}
     *
     *  @param msg The message that will replace the default failure message.
     *  @return    A parser with the same properties and different failure message.
     */
    def withFailureMessage(msg: String)(implicit loc: ParserLocation) = mkParser ({ in =>
      this(in) match {
        case Failure(_, next) => Failure(msg, next)
        case other            => other
      }
    }, loc)

    /** Changes the error message produced by a parser.
     *
     *  This doesn't change the behavior of a parser on neither
     *  success nor failure, just on error. The semantics are
     *  slightly different than those obtained by doing `| error(msg)`,
     *  in that the message produced by this method will always
     *  replace the message produced, which is not guaranteed
     *  by that idiom.
     *
     *  For example, parser `p` below will always produce the
     *  designated error message, while `q` will not produce
     *  it if `sign` is parsed but `number` is not.
     *
     *  {{{
     *  def p = sign.? ~ number withErrorMessage  "Number expected!"
     *  def q = sign.? ~ number | error("Number expected!")
     *  }}}
     *
     *  @param msg The message that will replace the default error message.
     *  @return    A parser with the same properties and different error message.
     */
    def withErrorMessage(msg: String)(implicit loc: ParserLocation) = mkParser ({ in =>
      this(in) match {
        case Error(_, next) => Error(msg, next)
        case other          => other
      }
    }, loc)
  }
}