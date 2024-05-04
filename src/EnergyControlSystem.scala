import scala.io.StdIn.readLine  // Ensure this is at the top of your file

object EnergyControlSystem extends App {
  // Instantiate power sources
  val solarPanel = new SolarPanel(1000)
  val windTurbine = new WindTurbine(1500)
  val hydropower = new Hydropower(2000)

  val sources = Map("solar" -> solarPanel, "wind" -> windTurbine, "hydro" -> hydropower)

  // Default logging interval and storage capacity
  var loggingInterval = 10
  val maxStorageCapacity = 100000.0

  var energyLogger = new EnergyLogger(sources, loggingInterval, maxStorageCapacity)

  // Start logging data
  energyLogger.logEnergyData()

  // Command loop for user input
  while (true) {
    println("Enter command (type 'help' for options): ")
    val input = readLine().toLowerCase.split("\\s+")

    input(0) match {
      case "help" =>
        println("Commands:")
        println("  adjust [source] [factor] - Adjust output by factor (e.g., 1.1 or 0.9)")
        println("  toggle [source] [on/off] - Toggle power source on or off")
        println("  interval [seconds] - Adjust data logging interval")
        println("  statistics - Show total and average output for each plant, and remaining storage")
        println("  status - Display current status of all sources and the current logging interval")
        println("  quit - Exit the program")
        println("  analyze - Analyze data for specific statistics")

      case "adjust" =>
        if (input.length == 3) {
          sources.get(input(1)).foreach(_.adjustOutput(input(2).toDouble))
          println(s"${input(1).capitalize} output adjusted.")
        } else {
          println("Invalid command or parameters.")
        }

      case "toggle" =>
        if (input.length == 3) {
          sources.get(input(1)).foreach(_.toggleOnOff(input(2) == "on"))
          println(s"${input(1).capitalize} turned ${input(2)}.")
        } else {
          println("Invalid command or parameters.")
        }

      case "interval" =>
        if (input.length == 2 && input(1).matches("\\d+")) {
          val newInterval = input(1).toInt
          energyLogger.updateInterval(newInterval)
          loggingInterval = newInterval
          println(s"Logging interval adjusted to $newInterval seconds.")
        } else {
          println("Invalid command or parameters.")
        }

      case "statistics" =>
        energyLogger.displayStatistics()
        energyLogger.checkAndAlertForLowOutput()

      case "status" =>
        println(s"Current logging interval: $loggingInterval seconds")
        println("Instantaneous output of all generators:")
        sources.foreach { case (key, source) =>
          val data = source.generateData()
          println(s"${data.energySource.capitalize}: Output = ${data.output}, Status = ${if (data.status) "On" else "Off"}")
        }
        energyLogger.checkAndAlertForLowOutput()

      case "quit" =>
        println("Exiting program.")
        energyLogger.stopLogging()
        System.exit(0)

      case "analyze" =>
        energyLogger.analyzeData()

      case _ =>
        println("Unknown command.")
    }
  }
}
