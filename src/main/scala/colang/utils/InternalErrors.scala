package colang.utils

import java.util.Locale

object InternalErrors {
  def missingPrelude: Nothing = {
    val message = Locale.getDefault.getLanguage match {
      case "be" => "Памылка: 'prelude.co' ня знойдзены. Калі ласка, усталюйце стандартную бібліятэку CO."
      case "ru" => "Ошибка: 'prelude.co' не найден. Пожалуйста, установите стандартную библиотеку CO."
      case "en" | _ => "Error: 'prelude.co' not found. Please install CO standard library."
    }

    System.err.println(message)
    sys.exit(2)
  }

  def primitiveTypeIsNotAType(typeName: String): Nothing = {
    val message = Locale.getDefault.getLanguage match {
      case "be" => s"Памылка: '$typeName' не з'яўляецца тыпам у стандартнай бібліятэцы. Калі ласка, праверце, ці CO " +
        s"усталяваны карэктна і няма новых абнаўленьняў."

      case "ru" => s"Ошибка: '$typeName' - не тип в стандартной библиотеке. Пожалуйста, проверьте, правильно ли " +
        s"установлен CO, и нет ли новых обновлений."

      case "en" | _ => s"Error: '$typeName' is not a type in the standard library. Please check if your " +
        s"CO installation is correct and up-to-date."
    }

    System.err.println(message)
    sys.exit(2)
  }

  def missingPrimitiveType(typeName: String): Nothing = {
    val message = Locale.getDefault.getLanguage match {
      case "be" => s"Памылка: ў стандартнай бібліятэцы няма акрэсьленьня тыпу '$typeName'. Калі ласка, праверце, ці CO " +
        s"усталяваны карэктна і няма новых абнаўленьняў."

      case "ru" => s"Ошибка: в стандартной библиотеке отсутствует определение типа '$typeName'. Пожалуйста, проверьте, " +
        s"правильно ли установлен CO, и нет ли новых обновлений."

      case "en" | _ => s"Error: '$typeName' type definition not found in the standard library. Please check if your " +
        s"CO installation is correct and up-to-date."
    }

    System.err.println(message)
    sys.exit(2)
  }

  def noNativeSymbol(name: String): Nothing = {
    val message = Locale.getDefault.getLanguage match {
      case "be" => s"Унутраная памылка ў кампілятары: адсутнічае нізкаўзроўневае прадстаўленьне для сімвала '$name'"
      case "ru" => s"Внутренняя ошибка в компиляторе: отсутствует низкоуровневое представление для символа '$name'"
      case "en" | _ => s"Internal compiler error: no native representation for symbol '$name'."
    }

    System.err.println(message)
    sys.exit(2)
  }
}
