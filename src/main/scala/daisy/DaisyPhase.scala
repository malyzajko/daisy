// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy


import lang.Trees.Program

/** One logical analysis/synthesis step, e.g. computing ranges, replacing trig functions.
 * This should be as immutable as possible so that we can (possibly) run it in parallel.
 */
trait DaisyPhase extends Pipeline[Program, Program] with Component