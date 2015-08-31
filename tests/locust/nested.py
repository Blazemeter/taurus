# An example on how to nest tasksets

from locust import HttpLocust, TaskSet, task


class ForumPage(TaskSet):
    pass


class AboutPage(TaskSet):
    pass


class WebsiteTasks(TaskSet):
    # We specify sub TaskSets using the tasks dict
    tasks = {
        ForumPage: 20,
        AboutPage: 10,
    }

    # We can use the @task decorator as well as the  
    # tasks dict in the same TaskSet
    @task(10)
    def index(self):
        pass


class WebsiteUser(HttpLocust):
    task_set = WebsiteTasks
    min_wait = 500
    max_wait = 1500
