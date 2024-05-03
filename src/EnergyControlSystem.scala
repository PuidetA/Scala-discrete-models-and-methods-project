import scala.io.StdIn.readLine

object EnergyControlSystem extends App {
  val solarPanel = new SolarPanel(1000)
  val windTurbine = new WindTurbine(1500)
  val hydropower = new Hydropower(2000)

  val sources = Map("solar" -> solarPanel, "wind" -> windTurbine, "hydro" -> hydropower)

  while (true) {
    println("Enter command (type 'help' for options): ")
    val input = readLine().toLowerCase.split("\\s+")

    input(0) match {
      case "help" =>
        println("Commands:")
        println("  adjust [source] [factor] - Adjust output by factor (e.g., 1.1 or 0.9)")
        println("  toggle [source] [on/off] - Toggle power source on or off")
        println("  status - Display current status of all sources")
        println("  quit - Exit the program")

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

      case "status" =>
        println("Note the following data are mocked to demonstrate instantaneous output for different generators.")
        sources.foreach { case (key, source) =>
          val data = source.generateData()
          println(s"${data.energySource.capitalize}: Output = ${data.output}, Status = ${if (data.status) "On" else "Off"}")
        }

      case "quit" =>
        println("Exiting program.")
        System.exit(0)

      case _ =>
        println("Unknown command.")
    }
  }
}