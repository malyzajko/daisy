
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */
package daisy

abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {
    def run(ctx: Context, v: F): (Context, G) = {
      val (ctx2, s) = self.run(ctx, v)
      //if(ctx.findOptionOrDefault(SharedOptions.optStrictPhases)) ctx.reporter.terminateIfError()
      thenn.run(ctx2, s)
    }
  }

  def run(ctx: Context, v: F): (Context, T)
}