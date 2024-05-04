import java.io.{BufferedWriter, FileWriter}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}

class EnergyLogger(sources: Map[String, EnergySource], var intervalSeconds: Int) {
  private val filePath = "energy_data.csv"
  @volatile private var isRunning = true

  // Writes header to the file
  private def writeHeader(writer: BufferedWriter): Unit = {
    writer.write("Timestamp,Source,Output,Status\n")
  }

  // Logs energy data to a CSV file
  def logEnergyData(): Unit = {
    Future {
      blocking {
        val writer = new BufferedWriter(new FileWriter(filePath, true))
        writeHeader(writer)

        while (isRunning) {
          sources.foreach { case (_, source) =>
            val data = source.generateData()
            val row = s"${data.timestamp},${data.energySource},${data.output},${data.status}\n"
            writer.write(row)
          }
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

  // Stops the logging process
  def stopLogging(): Unit = {
    isRunning = false
  }
}