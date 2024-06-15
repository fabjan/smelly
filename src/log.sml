(*
 * Copyright 2024 Fabian Bergström
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

structure Log =
struct

datatype level = DEBUG | INFO | WARN | ERROR

val currentLevel = ref INFO

fun setLevel level = currentLevel := level

fun ord DEBUG = 0
  | ord INFO = 1
  | ord WARN = 2
  | ord ERROR = 3

fun levelString DEBUG = "DEBUG"
  | levelString INFO = "INFO"
  | levelString WARN = "WARN"
  | levelString ERROR = "ERROR"

fun log level message =
  if ord (!currentLevel) <= ord level
  then print (levelString level ^ ": " ^ message ^ "\n")
  else ()

fun debug message = log DEBUG message
fun info message = log INFO message
fun warn message = log WARN message
fun error message = log ERROR message

end
