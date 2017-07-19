
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy


import lang.Trees.Program

/** One logical analysis/synthesis step, e.g. computing ranges, replacing trig functions.
 * This should be as immutable as possible so that we can (possibly) run it in parallel.
 */
trait DaisyPhase extends Pipeline[Program, Program] with Component {

}
