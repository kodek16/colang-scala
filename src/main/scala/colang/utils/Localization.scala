package colang.utils

import java.util.ResourceBundle

/**
  * A convenience object that handles message localization.
  */
object Localization {
  private val messages = ResourceBundle.getBundle("Messages")

  /**
    * Returns a localized message.
    * @param messageName name of the message to localize
    * @return localized message
    */
  def tr(messageName: String): String = messages.getString(messageName)
}
