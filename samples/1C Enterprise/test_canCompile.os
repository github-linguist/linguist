#Использовать "../libs/oscript-library/src/v8runner"
#Использовать "../libs/oscript-library/src/tempfiles"

Перем Лог;
Перем КодВозврата;

Процедура Инициализация()

	Лог = Логирование.ПолучитьЛог("oscript.app.gitlab-test_CanCompile");
	КодВозврата = 0;

КонецПроцедуры

Процедура ВыполнитьТест()

	Конфигуратор = Новый УправлениеКонфигуратором();

	ПараметрыЗапуска = Конфигуратор.ПолучитьПараметрыЗапуска();
	КомандаЗапуска = "/LoadConfigFromFiles ""%1""";
	КомандаЗапуска = СтрШаблон(КомандаЗапуска, ТекущийКаталог() + "\source\cf");

	Лог.Информация("Команда обновления конфигурации: " + КомандаЗапуска);

	ПараметрыЗапуска.Добавить(КомандаЗапуска);

	Попытка
	    Конфигуратор.ВыполнитьКоманду(ПараметрыЗапуска);
	Исключение

	    Лог.Ошибка(Конфигуратор.ВыводКоманды());
	    КодВозврата = 1;

	КонецПопытки;

	УдалитьФайлы(Конфигуратор.ПутьКВременнойБазе());

КонецПроцедуры

Инициализация();
ВыполнитьТест();

ЗавершитьРаботу(КодВозврата);