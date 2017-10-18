// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

abstract class Pipeline[-F, +T] {
  self =>

  def >>[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {
    def run(ctx: Context, v: F): (Context, G) = {
      val (ctx2, s) = self.run(ctx, v)
      // if(ctx.findOptionOrDefault(SharedOptions.optStrictPhases)) ctx.reporter.terminateIfError()
      thenn.run(ctx2, s)
    }
  }

  def run(ctx: Context, v: F): (Context, T)
}