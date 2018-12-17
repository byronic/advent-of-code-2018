import scala.io.Source

object AcyclicGraph {
case class Worker(step: Step, startTick: Int)

case class Dependency(name: String, satisfied: Boolean)

  case class Step(name: String, dependencies: List[Dependency]) {
    private val duration: Int = name match {
      case "A" => 61
      case "B" => 62
      case "C" => 63
      case "D" => 64
      case "E" => 65
      case "F" => 66
      case "G" => 67
      case "H" => 68
      case "I" => 69
      case "J" => 70
      case "K" => 71
      case "L" => 72
      case "M" => 73
      case "N" => 74
      case "O" => 75
      case "P" => 76
      case "Q" => 77
      case "R" => 78
      case "S" => 79
      case "T" => 80
      case "U" => 81
      case "V" => 82
      case "W" => 83
      case "X" => 84
      case "Y" => 85
      case "Z" => 86
      case _ => 0
      }

    def isReady(): Boolean = {
      def go(dependencies: List[Dependency]): Boolean = {
        if (dependencies.isEmpty) true
        else if (dependencies.head.satisfied) go(dependencies.tail)
        else false
      }
      go(this.dependencies)
    }

    def isDone(ticks: Int): Boolean = ticks >= duration
  }

  /* gets the two steps presented by the string; the first step returned is
     independent; the second step has the dependency on the first
  */
  def parseStep(s: String): (Step, Step) = {
    // requirements file strings look like this:
    //   "Step U must be finished before step R can begin. "
    val split: Array[String] = s.split(" ")
    val independent: Step = Step(split(1), List())
    val dependent: Step = Step(split(7), List(Dependency(independent.name, false)))
    (independent, dependent)
  }

  /**
    * Returns an updated list of steps with the 'union' step added to or updated
    * within the list
    */
  def updateInPlace(steps: List[Step], union: Step): List[Step] = {
    def go(steps: List[Step], updated: List[Step]): List[Step] = {
      if (steps.isEmpty) union :: updated
      else if (steps.head.name == union.name) {
        val newList: List[Dependency] = steps.head.dependencies ++ union.dependencies
        val newUpdated: List[Step] = Step(steps.head.name, newList) :: updated
        newUpdated ++ steps.tail
      } else go(steps.tail, steps.head :: updated)
    }
    go(steps, List())
  }

  def buildGraph(requirements: List[String]): List[Step] = {
    def go(requirements: List[String], steps: List[Step]): List[Step] = {
      if (requirements.isEmpty) steps
      else {
        val (ind, dep): (Step, Step) = parseStep(requirements.head)
        val intermediate: List[Step] = updateInPlace(steps, ind)
        go(requirements.tail, updateInPlace(intermediate, dep))
      }
    }
    go(requirements, List())
  }

  def updateSatisfiedDependencies(steps: List[Step], completed: Step): List[Step] = {
    def checkStep(step: Step): Step = {
      def go(dependencies: List[Dependency], updated: List[Dependency]): Step = {
        if (dependencies.isEmpty) Step(step.name, updated)
        else if (dependencies.head.name == completed.name) go(dependencies.tail, Dependency(completed.name, true) :: updated)
        else go(dependencies.tail, dependencies.head :: updated)
      }
      go(step.dependencies, List())
    }
    def go(steps: List[Step], updated: List[Step]): List[Step] = {
      if (steps.isEmpty) updated
      else go(steps.tail, checkStep(steps.head) :: updated)
    }
    go(steps, List())
  }

  def updateSatisfiedDependenciesList(steps: List[Step], completed: List[Step]): List[Step] = {
    if (completed.isEmpty) steps
    else updateSatisfiedDependenciesList(updateSatisfiedDependencies(steps, completed.head), completed.tail)
  }

  def clearExecuted(steps: List[Step], completed: Step): List[Step] = {
    def go(steps: List[Step], updated: List[Step]): List[Step] = {
      if (steps.isEmpty) updated
      else go(steps.tail, if (steps.head.name == completed.name) updated else steps.head :: updated)
    }
    go(steps, List())
  }

  def clearExecutedList(steps: List[Step], completed: List[Step]): List[Step] = {
    if (completed.isEmpty) steps
    else clearExecutedList(clearExecuted(steps, completed.head), completed.tail)
  }

  def executeNext(steps: List[Step]): Step = {
    def go(steps: List[Step], readySteps: List[Step]): Step = {
      if (steps.isEmpty) {
        if (readySteps.size == 1) readySteps(0)
        else readySteps.sortWith(_.name < _.name).head
      } else go(steps.tail, if (steps.head.isReady()) steps.head :: readySteps else readySteps)
    }
    go(steps, List())
  }

  def getAllExecutableSteps(steps: List[Step], workers: List[Worker]): List[Step] = {
    def checkWorkers(w: List[Worker], s: Step): Boolean = {
      if (w.isEmpty) false
      else if (w.head.step.name == s.name) true
      else checkWorkers(w.tail, s)
    }

    def go(steps: List[Step], readySteps: List[Step]): List[Step] = {
      if (steps.isEmpty) readySteps.sortWith(_.name < _.name)
      else if (steps.head.isReady()) {
        if (checkWorkers(workers, steps.head)) go(steps.tail, readySteps)
        else go(steps.tail, steps.head :: readySteps)
      } else {
        go(steps.tail, readySteps)
      }
    }
    go(steps, List())
  }

  def performExecution(steps: List[Step]): String = {
    def go(steps: List[Step], acc: String): String = {
      if (steps.isEmpty) acc
      else {
        val completedStep: Step = executeNext(steps)
        val updatedSteps: List[Step] = clearExecuted(updateSatisfiedDependencies(steps, completedStep), completedStep)
        go(updatedSteps, acc + completedStep.name)
      }
    }
    go(steps, "")
  }

  def performTimedExecution(steps: List[Step], maxWorkers: Int): String = {
    // get an updated list of active workers and completed steps
    def checkWorkers(ticks: Int, workers: List[Worker]): (List[Worker], List[Step]) = {
      def go(workers: List[Worker], updatedWorkers: List[Worker], completed: List[Step]): (List[Worker], List[Step]) = {
        if (workers.isEmpty) (updatedWorkers, completed)
        else if (workers.head.step.isDone(ticks - workers.head.startTick)) go(workers.tail, updatedWorkers, workers.head.step :: completed)
        else go(workers.tail, workers.head :: updatedWorkers, completed)
      }
      go(workers, List(), List())
    }
    def assignWork(s: List[Step], w: List[Worker], tick: Int): List[Worker] = {
      if (s.isEmpty || w.size == maxWorkers) w
      else assignWork(s.tail, Worker(s.head, tick) :: w, tick)
    }
    def completedString(c: List[Step], acc: String): String = {
      if (c.isEmpty) acc
      else completedString(c.tail, acc + c.head.name)
    }
    def go(currentTick: Int, remainingSteps: List[Step], activeWorkers: List[Worker], acc: String): String = {
      if(remainingSteps.isEmpty) acc + s", ${currentTick - 1}"
      else {
        // see if anyone completed jobs
        val res: (List[Worker], List[Step]) = checkWorkers(currentTick, activeWorkers)
        val updatedActiveWorkers: List[Worker] = res._1
        val completed: List[Step] = res._2
        if (completed.isEmpty) {
          go(currentTick + 1, remainingSteps, activeWorkers, acc)
        } else {
          val updatedRemainingSteps: List[Step] = clearExecutedList(updateSatisfiedDependenciesList(remainingSteps, completed), completed)
          val allExecutable: List[Step] = getAllExecutableSteps(updatedRemainingSteps, updatedActiveWorkers)
          val updatedWorkers: List[Worker] = assignWork(allExecutable, updatedActiveWorkers, currentTick)
          go(currentTick + 1, updatedRemainingSteps, updatedWorkers, acc + completedString(completed, ""))
        }
      }
    }
    val allExecutable: List[Step] = getAllExecutableSteps(steps, List())
    val workers: List[Worker] = assignWork(allExecutable, List(), 0)
    go(1, steps, workers, "")
  }


  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("dataset")
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    println(performExecution(buildGraph(lines)))
    println(performTimedExecution(buildGraph(lines), 5))
  }
}
