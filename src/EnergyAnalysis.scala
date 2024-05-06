import java.time.LocalDateTime

object EnergyAnalysis extends App {

  case class RenewableEnergyData(timestamp: LocalDateTime, energyOutput: Double)

  object DataAnalysis {
    def mean(data: Seq[RenewableEnergyData]): Double = {
      data.map(_.energyOutput).sum / data.length
    }

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
      data.groupBy(_.energyOutput)
        .maxBy(_._2.length)
        ._1
    }

    def range(data: Seq[RenewableEnergyData]): Double = {
      val sortedData = data.sortBy(_.energyOutput)
      sortedData.last.energyOutput - sortedData.head.energyOutput
    }

    def midrange(data: Seq[RenewableEnergyData]): Double = {
      val sortedData = data.sortBy(_.energyOutput)
      (sortedData.head.energyOutput + sortedData.last.energyOutput) / 2
    }
    def filterHourly(data: Seq[RenewableEnergyData], hour: Int): Seq[RenewableEnergyData] = {
      data.filter(_.timestamp.getHour == hour)
    }

    def filterDaily(data: Seq[RenewableEnergyData], day: Int): Seq[RenewableEnergyData] = {
      data.filter(_.timestamp.getDayOfMonth == day)
    }

    def filterWeekly(data: Seq[RenewableEnergyData]): Seq[RenewableEnergyData] = {
      val now = LocalDateTime.now()
      val startOfWeek = now.minusDays(now.getDayOfWeek.getValue - 1)
      data.filter(d => d.timestamp.isAfter(startOfWeek) && d.timestamp.isBefore(startOfWeek.plusWeeks(1)))
    }

    def filterMonthly(data: Seq[RenewableEnergyData], month: Int): Seq[RenewableEnergyData] = {
      data.filter(_.timestamp.getMonthValue == month)
    }

    def search(data: Seq[RenewableEnergyData], energyOutputTarget: Double): Seq[RenewableEnergyData] = {
      data.filter(_.energyOutput == energyOutputTarget)
    }
  }

  def performAnalysis(timeframe: String, calculation: String): Unit = {
    val filteredData = timeframe match {
      case "hourly" => DataAnalysis.filterHourly(energyData, LocalDateTime.now().getHour)
      case "daily" => DataAnalysis.filterDaily(energyData, LocalDateTime.now().getDayOfMonth)
      case "weekly" => DataAnalysis.filterWeekly(energyData)
      case "monthly" => DataAnalysis.filterMonthly(energyData, LocalDateTime.now().getMonthValue)
      case _ => println("Invalid timeframe."); return  
    }

    val finalCalculation = calculation match {
      case "mean" => println(s"Mean output: ${DataAnalysis.mean(filteredData)}")
      case "median" => println(s"Median output: ${DataAnalysis.median(filteredData)}")
      case "mode" => println(s"Mode output: ${DataAnalysis.mode(filteredData)}")
      case "range" => println(s"Range output: ${DataAnalysis.range(filteredData)}")
      case "midrange" => println(s"Midrange output: ${DataAnalysis.midrange(filteredData)}")
      case _ => println("Invalid calculation.")
    }
  }
    /* This is the main method that will be used to test the functions in the DataAnalysis object. You search by search() function.

    val readingsMatchingOutput = search(energyReadings, 12.0) 
  }

  val energyReadings = Seq(RenewableEnergyData(LocalDateTime.now(), 12.5),
    RenewableEnergyData(LocalDateTime.now().minusHours(1), 10.0),
    RenewableEnergyData(LocalDateTime.now().minusHours(3), 12.5), // Duplicate for mode testing
    RenewableEnergyData(LocalDateTime.now().minusDays(1), 15.2),
    RenewableEnergyData(LocalDateTime.now().minusDays(5), 8.7),
    RenewableEnergyData(LocalDateTime.now().minusDays(8), 10.5)
  )*/

/*
  // Test Mean
  val averageOutput = DataAnalysis.mean(energyReadings)
  println(s"Average energy output: $averageOutput") // Expect something around 11.56666

  // Test Median
  val medianOutput = DataAnalysis.median(energyReadings)
  println(s"Median energy output: $medianOutput")   // Expect 11.5

  // Test Mode
  val modeOutput = DataAnalysis.mode(energyReadings)
  println(s"Mode of energy output: $modeOutput")    // Expect 12.5

  // Test Range
  val outputRange = DataAnalysis.range(energyReadings)
  println(s"Range of energy output: $outputRange")  // Expect around 6.5

  // Test Midrange
  val outputMidRange = DataAnalysis.midrange(energyReadings)
  println(s"Midrange of energy output: $outputMidRange") // Expect around 11.95

  // Test Filtering - Example with hourly filter
  val currentHour = LocalDateTime.now().getHour
  val thisHourReadings = DataAnalysis.filterHourly(energyReadings, currentHour)
  println(s"Readings for the current hour: $thisHourReadings")

  // Test Search - Example searching for output of 10.0
  val matchingReadings = DataAnalysis.search(energyReadings, 10.0)
  println(s"Readings matching energy output 10.0: $matchingReadings")*/
}