import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}

class EnergyLogger(sources: Map[String, EnergySource], var intervalSeconds: Int, storageCapacity: Double) {
  private val filePath = "energy_data.csv"
  @volatile private var isRunning = true
  private var totalStorage = storageCapacity

  // Writes header to the file if the file is empty
  private def writeHeader(writer: BufferedWriter): Unit = {
    writer.write("Timestamp,Source,Output,Status\n")
  }

  // Logs energy data to a CSV file
  def logEnergyData(): Unit = {
    Future {
      blocking {
        val writer = new BufferedWriter(new FileWriter(filePath, true))
        // Write the header only if the file does not exist or is empty
        if (!Files.exists(Paths.get(filePath)) || Files.size(Paths.get(filePath)) == 0) {
          writeHeader(writer)
        }

        while (isRunning) {
          sources.foreach { case (_, source) =>
            val data = source.generateData()
            val row = s"${data.timestamp},${data.energySource},${data.output},${data.status}\n"
            writer.write(row)

            // Update storage if the source is active
            if (data.status) {
              totalStorage = Math.min(totalStorage + data.output, storageCapacity)
            }
          }

          // Consume a reasonable amount of storage (10% here for demonstration)
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

    // Skip header
    reader.readLine()
    var line = reader.readLine()

    while (line != null) {
      val columns = line.split(",")
      // Ensure the line is valid and contains numeric data
      if (columns.length == 4 && columns(2).forall(c => c.isDigit || c == '.')) {
        val Array(_, source, output, _) = columns
        val (totalOutput, count) = sourceStats(source)
        sourceStats(source) = (totalOutput + output.toDouble, count + 1)
      }
      line = reader.readLine()
    }
    reader.close()

    // Print statistics for each source
    sourceStats.foreach { case (source, (totalOutput, count)) =>
      val avgOutput = if (count == 0) 0.0 else totalOutput / count
      println(s"$source - Total Output: $totalOutput, Average Output: $avgOutput")
    }
    // Only display remaining storage during statistics
    println(s"Remaining Available Storage: $totalStorage")
  }
}
