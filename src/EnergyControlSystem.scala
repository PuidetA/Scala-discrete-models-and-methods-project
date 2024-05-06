import java.time.LocalDateTime
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.StdIn.readLine
import scala.io.{Source, StdIn}
import scala.util.Try


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

  // Case class representing renewable energy data
  case class RenewableEnergyData(timestamp: LocalDateTime, energyOutput: Double)

  // Function to read and parse CSV data into RenewableEnergyData instances
  def loadCsvData(): Seq[RenewableEnergyData] = {
    val csvFilePath = "energy_data.csv"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSS")
    val source = Source.fromFile(csvFilePath)

    val data = source.getLines().drop(1).flatMap { line =>
      val cols = line.split(",").map(_.trim)

      if (cols.length >= 4) {
        val timestampOpt = Try(LocalDateTime.parse(cols(0), formatter)).toOption
        val energyOutputOpt = Try(cols(2).toDouble).toOption
        val statusOpt = Try(cols(3).toBoolean).toOption

        (timestampOpt, energyOutputOpt, statusOpt) match {
          case (Some(timestamp), Some(energyOutput), Some(status)) =>
            Some(RenewableEnergyData(timestamp, energyOutput))
          case _ => None
        }
      } else None
    }.toSeq

    source.close()
    data
  }







  // Object containing analysis functions
  object DataAnalysis {
    def mean(data: Seq[RenewableEnergyData]): Double = data.map(_.energyOutput).sum / data.length

    def median(data: Seq[RenewableEnergyData]): Double = {
      val sortedData = data.sortBy(_.energyOutput)
      val mid = sortedData.length / 2
      if (sortedData.length % 2 == 0) {
        (sortedData(mid - 1).energyOutput + sortedData(mid).energyOutput) / 2
      } else {
        sortedData(mid).energyOutput
      }
    }

    def mode(data: Seq[RenewableEnergyData]): Double = {
      data.groupBy(_.energyOutput).maxBy(_._2.length)._1
    }

    def range(data: Seq[RenewableEnergyData]): Double = {
      val sortedData = data.sortBy(_.energyOutput)
      sortedData.last.energyOutput - sortedData.head.energyOutput
    }

    def midrange(data: Seq[RenewableEnergyData]): Double = {
      val sortedData = data.sortBy(_.energyOutput)
      (sortedData.head.energyOutput + sortedData.last.energyOutput) / 2
    }

    def filterMinutely(data: Seq[RenewableEnergyData], hour: Int, minute: Int): Seq[RenewableEnergyData] = {
      // Filter data based only on the hour and minute parts of the timestamp
      val filtered = data.filter { d =>
        d.timestamp.getHour == hour && d.timestamp.getMinute == minute
      }

      // Debug print to inspect the data matching the specific hour and minute
      println(s"Data available for hour $hour and minute $minute: ${filtered.map(_.timestamp)}")
      filtered
    }



    def filterHourly(data: Seq[RenewableEnergyData], hour: Int): Seq[RenewableEnergyData] =
      data.filter(_.timestamp.getHour == hour)

    def filterDaily(data: Seq[RenewableEnergyData], day: Int): Seq[RenewableEnergyData] =
      data.filter(_.timestamp.getDayOfMonth == day)

    def filterWeekly(data: Seq[RenewableEnergyData]): Seq[RenewableEnergyData] = {
      val now = LocalDateTime.now()
      val startOfWeek = now.minusDays(now.getDayOfWeek.getValue - 1)
      data.filter(d => d.timestamp.isAfter(startOfWeek) && d.timestamp.isBefore(startOfWeek.plusWeeks(1)))
    }

    def filterMonthly(data: Seq[RenewableEnergyData], month: Int): Seq[RenewableEnergyData] =
      data.filter(_.timestamp.getMonthValue == month)

    def search(data: Seq[RenewableEnergyData], energyOutputTarget: Double): Seq[RenewableEnergyData] =
      data.filter(_.energyOutput == energyOutputTarget)
  }

  // Analysis function prompts user and executes analysis based on their input
  def performAnalysis(timeframe: String, calculation: String): Unit = {
    val data = loadCsvData()

    val filteredData = timeframe match {
      case "minutely" =>
        println("Enter the hour (0-23):")
        val hour = StdIn.readInt()
        println("Enter the minute (0-59):")
        val minute = StdIn.readInt()
        DataAnalysis.filterMinutely(data, hour, minute)

      case "hourly" =>
        println("Enter the hour (0-23):")
        val hour = StdIn.readInt()
        DataAnalysis.filterHourly(data, hour)

      case "daily" =>
        println("Enter the day of the month:")
        val day = StdIn.readInt()
        DataAnalysis.filterDaily(data, day)

      case "weekly" => DataAnalysis.filterWeekly(data)
      case "monthly" =>
        println("Enter the month (1-12):")
        val month = StdIn.readInt()
        DataAnalysis.filterMonthly(data, month)

      case _ =>
        println("Invalid timeframe.")
        return
    }

    if (filteredData.isEmpty) {
      println(s"No data available for the specified $timeframe period.")
      return
    }

    calculation match {
      case "mean" => println(s"Mean output: ${DataAnalysis.mean(filteredData)}")
      case "median" => println(s"Median output: ${DataAnalysis.median(filteredData)}")
      case "mode" => println(s"Mode output: ${DataAnalysis.mode(filteredData)}")
      case "range" => println(s"Range output: ${DataAnalysis.range(filteredData)}")
      case "midrange" => println(s"Midrange output: ${DataAnalysis.midrange(filteredData)}")
      case _ => println("Invalid calculation.")
    }
  }

  // Command loop for user input
  while (true) {
    println("Enter command (type 'help' for options): ")
    val input = readLine().toLowerCase.split("\\s+")

    input(0) match {
      case "help" =>
        println("Commands:")
        println("  adjust [source] [factor] - Adjust output by factor (e.g., 1.1 or 0.9)")
        println("  analysis [minutely/hourly/daily/weekly/monthly] [mean/median/mode/range/midrange] - Perform analysis on energy data")
        println("  toggle [source] [on/off] - Toggle power source on or off")
        println("  interval [seconds] - Adjust data logging interval")
        println("  statistics - Show total and average output for each plant, and remaining storage")
        println("  status - Display current status of all sources and the current logging interval")
        println("  quit - Exit the program")

      case "adjust" =>
        if (input.length == 3) {
          sources.get(input(1)).foreach(_.adjustOutput(input(2).toDouble))
          println(s"${input(1).capitalize} output adjusted.")
        } else {
          println("Invalid command or parameters.")
        }

      case "analysis" =>
        if (input.length == 3) {
          performAnalysis(input(1), input(2))
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

      case "status" =>
        println(s"Current logging interval: $loggingInterval seconds")
        println("Instantaneous output of all generators:")
        sources.foreach { case (key, source) =>
          val data = source.generateData()
          println(s"${data.energySource.capitalize}: Output = ${data.output}, Status = ${if (data.status) "On" else "Off"}")
        }

      case "quit" =>
        println("Exiting program.")
        energyLogger.stopLogging()
        System.exit(0)

      case _ =>
        println("Unknown command.")
    }
  }
}