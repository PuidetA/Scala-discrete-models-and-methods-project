import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.io.StdIn.readLine

class EnergyLogger(sources: Map[String, EnergySource], var intervalSeconds: Int, storageCapacity: Double) {
  private val filePath = "energy_data.csv"
  @volatile private var isRunning = true
  private var totalStorage = storageCapacity

  // Writes header to the file if the file is empty
  private def writeHeader(writer: BufferedWriter): Unit = {
    writer.write("Timestamp,Source,Output,Status\n")
  }

  // Method to check for low output and alert
  def checkAndAlertForLowOutput(): Unit = {
    sources.foreach { case (sourceName, source) =>
      val data = source.generateData()
      val lowOutputThreshold = 500.0  // Threshold for low output alert
      if (data.output < lowOutputThreshold && data.status) {
        println(s"ALERT: Critically low output from ${data.energySource}. Immediate attention required!")
      }
    }
  }

  // Logs energy data to a CSV file
  def logEnergyData(): Unit = {
    Future {
      blocking {
        val writer = new BufferedWriter(new FileWriter(filePath, true))
        if (!Files.exists(Paths.get(filePath)) || Files.size(Paths.get(filePath)) == 0) {
          writeHeader(writer)
        }

        while (isRunning) {
          sources.foreach { case (_, source) =>
            val data = source.generateData()
            val row = s"${data.timestamp},${data.energySource},${data.output},${data.status}\n"
            writer.write(row)

            if (data.status) {
              totalStorage = Math.min(totalStorage + data.output, storageCapacity)
            }
          }
          checkAndAlertForLowOutput()
          val consumption = totalStorage * 0.1
          totalStorage = Math.max(0, totalStorage - consumption)

          writer.flush()
          Thread.sleep(intervalSeconds * 1000)
        }
        writer.close()
      }
    }
  }

  // Updates the logging interval dynamically
  def updateInterval(newIntervalSeconds: Int): Unit = {
    intervalSeconds = newIntervalSeconds
  }

  // Returns the remaining storage
  def getRemainingStorage: Double = totalStorage

  // Stops the logging process
  def stopLogging(): Unit = {
    isRunning = false
  }

  // Computes and displays averages and totals from the log
  def displayStatistics(): Unit = {
    val sourceStats = mutable.Map[String, (Double, Int)]().withDefaultValue((0.0, 0))
    val reader = new BufferedReader(new FileReader(filePath))
    reader.readLine() // Skip header
    var line = reader.readLine()

    while (line != null) {
      val columns = line.split(",")
      if (columns.length == 4 && columns(2).forall(c => c.isDigit || c == '.')) {
        val Array(_, source, output, _) = columns
        val (totalOutput, count) = sourceStats(source)
        sourceStats(source) = (totalOutput + output.toDouble, count + 1)
      }
      line = reader.readLine()
    }
    reader.close()

    sourceStats.foreach { case (source, (totalOutput, count)) =>
      val avgOutput = if (count == 0) 0.0 else totalOutput / count
      println(s"$source - Total Output: $totalOutput, Average Output: $avgOutput")
    }
    println(s"Remaining Available Storage: $totalStorage")
  }

  // Method to perform data analysis
  def analyzeData(): Unit = {
    println("Enter time period (hourly/daily/weekly/monthly): ")
    val period = readLine().trim.toLowerCase
    println("Enter data type (mean/median/mode/range/midrange): ")
    val dataType = readLine().trim.toLowerCase

    val filteredData = filterDataByPeriod(period)
    dataType match {
      case "mean" => println(s"Mean: ${calculateMean(filteredData)}")
      case "median" => println(s"Median: ${calculateMedian(filteredData)}")
      case "mode" => println(s"Mode: ${calculateMode(filteredData)}")
      case "range" => println(s"Range: ${calculateRange(filteredData)}")
      case "midrange" => println(s"Midrange: ${calculateMidrange(filteredData)}")
      case _ => println("Invalid data type.")
    }
  }

  def filterDataByPeriod(period: String): List[Double] = {
    // This should be replaced with actual data retrieval and filtering logic based on 'period'
    List() // Placeholder
  }

  def calculateMean(data: List[Double]): Double = data.sum / data.length

  def calculateMedian(data: List[Double]): Double = {
    val sorted = data.sorted
    if (sorted.size % 2 == 1) sorted(sorted.size / 2)
    else (sorted(sorted.size / 2 - 1) + sorted(sorted.size / 2)) / 2.0
  }

  def calculateMode(data: List[Double]): Double = {
    data.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
  }

  def calculateRange(data: List[Double]): Double = {
    data.max - data.min
  }

  def calculateMidrange(data: List[Double]): Double = {
    (data.min + data.max) / 2.0
  }
}
