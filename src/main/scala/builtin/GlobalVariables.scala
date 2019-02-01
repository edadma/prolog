package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{Structure, VM, instantiationError, typeError}

import scala.collection.mutable


object GlobalVariables {

  val vars = new mutable.HashMap[Symbol, Any]



}