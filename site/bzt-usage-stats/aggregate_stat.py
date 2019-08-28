from datetime import date, timedelta, datetime
import csv
import json
import os

STATISTICS_DIR = '/var/www/html/bzt-usage-stats/'

def counts_launches_per_day(filename, day, users_list, stats_list):
    """Counts launches per day"""

    yesterday = day
    stats_dict = stats_list[len(stats_list)-1]

    new_users_counter = 0
    cloud_launch_counter = 0
    desktop_launch_counter = 0

    environments = ["jenkins", "travis", "bamboo", "teamcity", "docker", "amazon", "google_cloud", "azure", "linux", "windows", "macos"]
    cloud_users_env = ["jenkins", "travis", "bamboo", "teamcity", "docker", "amazon", "google_cloud", "azure"]
    desktop_users_env = ["linux", "windows", "macos"]

    if len(stats_dict) == 0 or stats_dict['date'] == (yesterday-timedelta(days=1)).strftime('%d.%m.%Y'):
        stats_dict = {'date': yesterday.strftime('%d.%m.%Y')}
    else:
        stats_dict = stats_list.pop()

    with open(filename, "r") as read_file:
        reader = csv.reader(read_file)

        for row in reader:
            user = row[4]
            env = row[4].split('-')[0]

            if not user in users_list:
                new_users_counter += 1
                users_list.append(row[4])

            if env in environments:
                stats_dict.update({env: int(0 if stats_dict.get(env) is None else stats_dict.get(env))+1})

                if env in cloud_users_env:
                    cloud_launch_counter += 1
                elif env in desktop_users_env:
                    desktop_launch_counter += 1

    stats_dict.update({'cloud_launch': cloud_launch_counter, 'desktop_launch': desktop_launch_counter, 'new_users': new_users_counter})
    stats_list.append(stats_dict)

def counts_launches_per_week(days_list, weeks_list):
    """Counts launches per week"""

    first_day = date.today() - timedelta(days=7)
    previous_week = first_day.isocalendar()[1]

    week_dict = {}

    for index in range(len(days_list)-1, len(days_list)-8,-1):
        if index == -1:
            break

        day_dict = days_list[index]

        for elm in day_dict.keys():
            if elm == 'date':
                continue

            week_dict.update({elm: int(0 if week_dict.get(elm) is None else week_dict.get(elm))+int(day_dict.get(elm))})

    week_dict.update({'date': first_day.strftime('%d.%m.%Y'), 'week': previous_week})
    weeks_list.append(week_dict)

def counts_launches_per_month(days_list, months_list):
    """Counts launches per month"""

    previous_month = (date.today() - timedelta(days=1)).month
    previous_year = (date.today() - timedelta(days=1)).year
    month = "0" + str(previous_month) + "." + str(previous_year) if previous_month < 10 else str(previous_month) + "." + str(previous_year)

    month_dict = {}

    for index in range(len(days_list)-1, len(days_list)-32, -1):
        if index == -1:
            break

        day_dict = days_list[index]

        if not int(day_dict.get('date').split('.')[1]) == previous_month:
            break

        for elm in day_dict.keys():
            if elm == 'date':
                continue

            month_dict.update({elm: int(0 if month_dict.get(elm) is None else month_dict.get(elm))+int(day_dict.get(elm))})

    month_dict.update({'month': month})
    months_list.append(month_dict)

def list_to_json(list_name, json_name):
    """Serialization python collections to json"""

    with open(STATISTICS_DIR + json_name, "w") as write_file:
        json.dump(list_name, write_file)

def json_to_list(json_name):
    """Deserialization json to python collections"""

    with open(STATISTICS_DIR + json_name, "r") as read_file:
        data = json.load(read_file)

    return data

def main():
    """Aggregates raw data"""
    yesterday = date.today() - timedelta(days=1)
    csvname = STATISTICS_DIR + "stats_"+yesterday.strftime('%d.%m.%Y')+".csv"

    home_users = json_to_list("home_users.json")
    daily_stats = json_to_list("stats_by_day.json")
    counts_launches_per_day(csvname, yesterday, home_users, daily_stats)
    list_to_json(home_users, "home_users.json")
    list_to_json(daily_stats, "stats_by_day.json")

    if date.today().isocalendar()[2] == 1:
        weekly_stats = json_to_list("stats_by_week.json")
        counts_launches_per_week(daily_stats, weekly_stats)
        list_to_json(weekly_stats, "stats_by_week.json")

    if datetime.now().day == 1:
        monthly_stats = json_to_list("stats_by_month.json")
        counts_launches_per_month(daily_stats, monthly_stats)
        list_to_json(monthly_stats, "stats_by_month.json")

if __name__ == "__main__":
    main()