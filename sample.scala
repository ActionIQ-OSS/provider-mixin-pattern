import scala.collection.mutable.Map

class Evaluator(cachedEval: CachedEval) {
  def eval(expr: String): Int = cachedEval.eval(expr)
}

trait EvaluatorProvider {
    deps: CachedEvalProvider =>
  final def evaluator = new Evaluator(deps.cachedEval)
}


class CachedEval(cache: Map[String, Int], tokenizer: Tokenizer,
    tokenEval: TokenEval) {
  def eval(expr: String): Int = cache.getOrElse(expr, {
    val tokens = tokenizer.tokenize(expr)
    val result = tokenEval.eval(tokens)
    cache += expr -> result
    result
  })
}

trait CachedEvalProvider {
    deps: TokenizerProvider with TokenEvalProvider =>
  final def cachedEval = new CachedEval(
      Map(), deps.tokenizer, deps.tokenEval)
}


object Tokenizer { type Tokens = Array[String] }
class Tokenizer {
  def tokenize(expr: String): Tokenizer.Tokens = expr split " "
}

trait TokenizerProvider {
  final def tokenizer = new Tokenizer
}


/** Interface */
trait TokenEval {
  def eval(tokens: Tokenizer.Tokens): Int
}

/** Provider interface */
trait TokenEvalProvider {
  def tokenEval: TokenEval
}


/** Super simple evaluation */
class TokenEvalImpl extends TokenEval {
  override def eval(t: Tokenizer.Tokens): Int = t(1) match {
    case "+" => t(0).toInt + t(2).toInt
    case "*" => t(0).toInt * t(2).toInt
  }
}  

/** Provides the simple evaluator */
trait TokenEvalProviderImpl extends TokenEvalProvider {
  override final def tokenEval: TokenEval = new TokenEvalImpl
}


/** Always return 7 */
class TokenEval7Impl extends TokenEval {
  override final def eval(tokens: Tokenizer.Tokens) = 7
}

/** Provides the 7 evaluator */
trait TokenEval7ProviderImpl extends TokenEvalProvider {
  override final def tokenEval: TokenEval = new TokenEval7Impl
}


object Injector extends EvaluatorProvider
                   with CachedEvalProvider
                   with TokenizerProvider
                   with TokenEvalProviderImpl

object Main {
  def main(args: Array[String]) {
    val evaluator = Injector.evaluator
    println(evaluator.eval("1 + 1"))
    println(evaluator.eval("7 * 6"))
  }
}

