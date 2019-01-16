package xyz.hyperreal.prolog.builtin

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.time.{Instant, ZonedDateTime}

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{VM, domainError, typeError}


object Time {

  def timestamp( vm: VM, pos: IndexedSeq[Reader], now: Any ) = vm.unify( now, ZonedDateTime.now )

  def instant( vm: VM, pos: IndexedSeq[Reader], now: Any ) = vm.unify( now, Instant.now )

  def timeFormatter( vm: VM, pos: IndexedSeq[Reader], format: Any, formatter: Any ) =
    format match {
      case s: String =>
        try {
          val f = DateTimeFormatter.ofPattern( s )

          vm.unify( f, formatter )
        } catch {
          case e: IllegalArgumentException => domainError( pos(0), e.getMessage, 'datetime_format, format, 'timeFormatter, 1 )
        }
      case _ => typeError( pos(0), "expected Scala date time format string", 'string, format, 'timeFormatter, 1 )
    }

  def formatTime( vm: VM, pos: IndexedSeq[Reader], time: Any, format: Any, formatted: Any ) =
    (time, format) match {
      case (t: TemporalAccessor, f: DateTimeFormatter) => vm.unify( f format t, formatted )
      case _ => typeError( pos(0), "expected Java time and date time formatter", 'string, format, 'formatTime, 2 )
    }

}