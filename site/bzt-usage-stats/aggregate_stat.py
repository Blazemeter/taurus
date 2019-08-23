from datetime import date, timedelta, datetime
import csv
import json
import os

STATISTICS_DIR = '/var/www/html/bzt-usage-stats/'

def counts_launches_per_day(filename):
    """Counts launches per day"""
    date = None
    cloud_users_env = ["jenkins", "travis", "bamboo", "teamcity", "docker", "amazon", "google_cloud", "azure"]
    desktop_users_env = ["linux", "windows", "macos"]

    with open(filename, "r") as read_file:
        reader = csv.reader(read_file)
        cloud_launch_counter = 0
        desktop_launch_counter = 0

        for row in reader:
            env = row[4].split('-')[0]

            if date == None:
                date = row[0].split()[0]

            if env in cloud_users_env:
                cloud_launch_counter += 1
            elif env in desktop_users_env:
                desktop_launch_counter += 1

    return {date: {'cloud_count': cloud_launch_counter, 'desktop_count': desktop_launch_counter}}

def counts_launches_per_week(day_dict, new_users_dict, week_dict):
    """Counts launches per week"""

    current_year = str(datetime.now().year)
    first_day = date.today() - timedelta(days=7)
    previous_week = first_day.isocalendar()[1]
    previous_weeks_dict = week_dict.get(current_year)

    cloud_launch_counter = 0
    desktop_launch_counter = 0
    new_users_launch_counter = 0

    next_day = first_day
    while next_day != date.today():
        cloud_launch_counter += day_dict.get(next_day.strftime('%d.%m.%Y')).get('cloud_count')
        desktop_launch_counter += day_dict.get(next_day.strftime('%d.%m.%Y')).get('desktop_count')
        new_users_launch_counter += new_users_dict.get(next_day.strftime('%d.%m.%Y'))
        next_day = next_day + timedelta(days=1)

    previous_weeks_dict.update({previous_week: {'cloud_count': cloud_launch_counter, 'desktop_count': desktop_launch_counter, 'new_users_count': new_users_launch_counter}})

    return {current_year: previous_weeks_dict}

def counts_launches_per_month(day_dict, new_users_dict, month_dict):
    """Counts launches per month"""

    previous_month = (date.today() - timedelta(days=1)).month
    previous_year = (date.today() - timedelta(days=1)).year
    month = "0" + str(previous_month) + "." + str(previous_year)
    cloud_launch_counter = 0
    desktop_launch_counter = 0
    new_users_counter = 0

    for day in day_dict.keys():
        if int(day.split('.')[1]) == previous_month:
            cloud_launch_counter += day_dict[day].get('cloud_count')
            desktop_launch_counter += day_dict[day].get('desktop_count')
            new_users_counter += new_users_dict[day]

    return {month: {'cloud_count': cloud_launch_counter, 'desktop_count': desktop_launch_counter, 'new_users_count': new_users_counter}}


def counts_launches_by_platform(filename, dict_name):
    """Counts launches by platform"""

    with open(filename, "r") as read_file:
        reader = csv.reader(read_file)
        environments = ["jenkins", "travis", "bamboo", "teamcity", "docker", "amazon", "google_cloud", "azure", "linux", "windows", "macos"]

        for row in reader:
            env = row[4].split('-')[0]
            if env in environments:
                dict_name.update({env: int(0 if dict_name.get(env) is None else dict_name.get(env))+1})

def counts_new_users_per_day(filename, dict_name):
    """Counts new users per day and add users which use local machine"""
    date = None
    new_users_counter = 0

    with open(filename, "r") as read_file:
        reader = csv.reader(read_file)

        for row in reader:
            if (date == None):
                date = row[0].split()[0]

            if (dict_name.get(row[4]) != True ):
                new_users_counter += 1
                dict_name.update({row[4]: True})

    return {date: new_users_counter}

def dict_to_json(dict_name, json_name):
    """Serialization python collections to json"""

    with open(STATISTICS_DIR + json_name, "w") as write_file:
        json.dump(dict_name, write_file)

    return "file upload"

def json_to_dict(json_name):
    """Deserialization json to python collections"""

    with open(STATISTICS_DIR + json_name, "r") as read_file:
        data = json.load(read_file)

    return data

def main():
    """Aggregates raw data"""
    yesterday = date.today() - timedelta(days=1)
    csvname = STATISTICS_DIR + "stats_"+yesterday.strftime('%d.%m.%Y')+".csv"

    launch_by_day = json_to_dict("launch_by_day.json")
    launch_by_day.update(counts_launches_per_day(csvname))
    dict_to_json(launch_by_day, "launch_by_day.json")

    launch_by_platform = json_to_dict("launch_by_platform.json")
    counts_launches_by_platform(csvname, launch_by_platform)
    dict_to_json(launch_by_platform, "launch_by_platform.json")

    home_users = json_to_dict("home_users.json")
    new_users_by_day = json_to_dict("new_users_by_day.json")
    new_users_by_day.update(counts_new_users_per_day(csvname, home_users))
    dict_to_json(home_users, "home_users.json")
    dict_to_json(new_users_by_day, "new_users_by_day.json")

    if date.today().isocalendar()[2] == 1:
        launch_by_week = json_to_dict("launch_by_week.json")
        launch_by_week.update(counts_launches_per_week(launch_by_day, new_users_by_day, launch_by_week))
        dict_to_json(launch_by_week, "launch_by_week.json")

    if datetime.now().day == 1:
        launch_by_month = json_to_dict("launch_by_month.json")
        launch_by_month.update(counts_launches_per_month(launch_by_day, new_users_by_day, launch_by_month))
        dict_to_json(launch_by_month, "launch_by_month.json")

if __name__ == "__main__":
    main()