#данная программа была написана для автоматизации получения из новых таблиц записей, не имеющихся в старой базе
import os
import pandas as pd

new_data = pd.DataFrame(columns=['ID', 'Обращение','Имя','Фамилия','Отчество', 'Имя, Фамилия', 'Дата рождения','Фотография','Компания','Ответственный','Рабочий телефон','Мобильный телефон','Номер факса','Домашний телефон','Номер пейджера','Телефон для рассылок','Другой телефон','Корпоративный сайт','Личная страница','Страница Facebook','Страница ВКонтакте','Страница LiveJournal','Микроблог Twitter','Другой сайт','Рабочий e-mail','Частный e-mail','E-mail для рассылок','Другой e-mail','Контакт Facebook','Контакт Telegram','Контакт ВКонтакте','Контакт Skype','Контакт Viber','Комментарии Instagram','Контакт Битрикс24.Network','Онлайн-чат','Контакт Открытая линия','Контакт ICQ','Контакт MSN/Live!','Контакт Jabber','Другой контакт','Должность','Комментарий','Тип контакта','Источник','Дополнительно об источнике','Экспорт','Доступен для всех','Партнерская функция (для типа Партнеры)','Отношение к СБЛ-ГРУПП','Основное направление деятельности','Размер аудитории (количество врачей)','Кафедра','Лектор/тема','Ученая степень','WhatsApp','Пол','Темы','Только узкоспециальная аудитрия','Сотрудничество с компанией','Препарат','Приоритетный регион продвижения','Населенный пункт','Общий комментарий к потребности контакта','Подразделение компании','Специальность','Должность (база)','Планирование бюджета ВЕСНА','Сумма бюджета ВЕСНА','Планирование бюджета ОСЕНЬ','Сумма бюджета ОСЕНЬ','Регион ответственности','Имеющееся сотрудничество с OL','Лидер мнений','Желаемое сотрудничество с OL','Не желательный OL','Комментарий по OL','Согласие на обработку персональных данных','С какой вероятностью порекомендуют посещение наших мероприятий своим коллегам','Оцените, пожалуйста, мероприятие в целом'])

new_report_file = input("Введите название нового файла (отчета): ")
new_report_path = os.path.abspath(new_report_file)
new_report = pd.read_excel(new_report_path)
new_report = new_report.drop_duplicates(subset='Телефон')
new_report = new_report.drop_duplicates(subset='Email')

source = str(input('Введите источник контактов (например, "Вебинар"): '))
export = str(input('Экспорт "да" или "нет"?: '))
access = str(input('Доступ "да" или "нет"?: '))

name_col = list(new_report['Имя'])
surname_col = list(new_report['Фамилия'])
otchestvo_col = list(new_report['Отчество'])
#work_place_col для новых 
work_place_col = list(new_report['Организация / ЛПУ'])
#work_place_col = list(new_report['ЛПУ/Организция'])
#work_place_col для старых 
#work_place_col = list(new_report['Компания'])
number_col = list(new_report['Телефон'])
mail_col = list(new_report['Email'])
# city_col для новых 
city_col = list(new_report['Город / Округ'])
#city_col для старых
#city_col = list(new_report['Город'])

speciality_col = list(new_report['Специальность'])
source_col = [source]*len(name_col)
export_col = [export]*len(name_col)
access_col = [access]*len(name_col)

new_data['Имя'] = name_col
new_data['Фамилия'] = surname_col
new_data['Имя, Фамилия'] = new_data['Имя'].map(str) + " " + new_data['Фамилия'].map(str)
new_data['Отчество'] = otchestvo_col
new_data['Компания'] = work_place_col
new_data['Мобильный телефон'] = number_col
new_data['Рабочий e-mail'] = mail_col
new_data['Населенный пункт'] = city_col
new_data['Основное направление деятельности'] = speciality_col
new_data['Источник'] = source_col
new_data['Экспорт'] = export_col
new_data['Доступен для всех'] = access_col

new_data = new_data.drop_duplicates(subset=['Мобильный телефон'])
new_data = new_data.drop_duplicates(subset=['Рабочий e-mail'])
len(new_data)

new_data.head(2)

base_data_file = input("Введите название файла с текущей базой из Битрикса: ")
base_data_path = os.path.abspath(base_data_file)
base_data = pd.read_excel(base_data_path)

#убрали контакты с почтами, которые были в старой базе
common = new_data.merge(base_data, how = 'inner', on = ['Рабочий e-mail'] )
print("Общие строки: " + str(len(common)))

new_mail_data = new_data[~new_data['Рабочий e-mail'].isin(common['Рабочий e-mail'])] 
new_mail_data = new_mail_data.drop_duplicates()
print("Новые строки (по почте): " + str(len(new_mail_data)))

#убрали контакты с телефонами, которые были в старой базе
common = new_mail_data.merge(base_data, how = 'inner', on = ['Мобильный телефон'] )
print("Общие строки: " + str(len(common)))

new_data_to_add = new_mail_data[~new_mail_data['Мобильный телефон'].isin(common['Мобильный телефон'])] 
new_data_to_add = new_data_to_add.drop_duplicates()
print("Новые строки (по тлф): " + str(len(new_data_to_add)))


new_data_to_add.to_excel("/content/drive/MyDrive/импорт врачей в БК/для импорта/новые/для импорта 29.12.xlsx", index = False)

#добавляем новые контакты к старому файлу с базой
updated_base_data = pd.DataFrame(columns = base_data.columns.tolist())


updated_base_ID_col = list(base_data['ID']) + list(new_data_to_add['ID'])
updated_base_name_col = list(base_data['Имя']) + list(new_data_to_add['Имя']) 
updated_base_surname_col = list(base_data['Фамилия']) + list(new_data_to_add['Фамилия'])
updated_base_otchestvo_col = list(base_data['Отчество']) + list(new_data_to_add['Отчество'])
updated_base_work_place_col = list(base_data['Компания']) + list(new_data_to_add['Компания'])
updated_base_number_col = list(base_data['Мобильный телефон']) + list(new_data_to_add['Мобильный телефон'])
updated_base_mail_col = list(base_data['Рабочий e-mail']) + list(new_data_to_add['Рабочий e-mail'])
updated_base_city_col = list(base_data['Населенный пункт']) + list(new_data_to_add['Населенный пункт'])
updated_base_contact_type_col = list(base_data['Тип контакта']) + list(new_data_to_add['Тип контакта'])
updated_base_source_col = list(base_data['Источник']) + list(new_data_to_add['Источник'])
updated_base_export_col = list(base_data['Экспорт']) + list(new_data_to_add['Экспорт'])
updated_base_access_col = list(base_data['Делать контакт доступным']) + list(new_data_to_add['Доступен для всех'])
updated_base_speciality_col = list(base_data['Основное направление деятельности']) + list(new_data_to_add['Основное направление деятельности'])

updated_base_data['ID'] = updated_base_ID_col
updated_base_data['Имя'] = updated_base_name_col
updated_base_data['Фамилия'] = updated_base_surname_col
updated_base_data['Отчество'] = updated_base_otchestvo_col
updated_base_data['Компания'] = updated_base_work_place_col
updated_base_data['Мобильный телефон'] = updated_base_number_col
updated_base_data['Рабочий e-mail'] = updated_base_mail_col
updated_base_data['Населенный пункт'] = updated_base_city_col
updated_base_data['Тип контакта'] = updated_base_contact_type_col
updated_base_data['Источник'] = updated_base_source_col
updated_base_data['Экспорт'] = updated_base_export_col
updated_base_data['Доступен для всех'] = updated_base_access_col
updated_base_data['Основное направление деятельности'] = updated_base_speciality_col

updated_base_data.to_excel("/content/drive/MyDrive/импорт врачей в БК/norm_upd_base29.xlsx", index = False)
len(updated_base_data)

#после обработки new_data_to_add макросами (для заполнения направления деятельности и региона) итоговый файл конвертируем в csv 
clean_new_data_to_add = pd.read_excel("/content/drive/MyDrive/импорт врачей в БК/для импорта/новые/для импорта 29.12.xlsx")
clean_new_data_to_add.to_csv("/content/drive/MyDrive/импорт врачей в БК/для импорта/новые/для импорта 29.12.csv", encoding = "utf-8-sig", sep = ";",  index = False)
