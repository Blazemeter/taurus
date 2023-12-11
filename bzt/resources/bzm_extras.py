import requests
import uuid

from apiritif import get_from_thread_store

from bzt import TaurusException


# Allows for calling Blazemeter Test Data API Orchestration (publish, un-publish)
class BzmExtras(object):

    def __init__(self, settings=None):
        self.settings = settings
        self.publish_map = {}

    def do_testdata_orchestration(self, operation, entity_name, target_name="", pass_row_index=False):
        if self.settings is None or self.settings.get('master_publish_url') is None:
            raise TaurusException("Can't perform Test Data Orchestration. API Publish URL was not provided to the "
                                  "test!")
        if operation not in ["publish", "un-publish"]:
            raise TaurusException(f"Invalid operation for publishing, must be either 'publish' or 'un-publish', "
                                  f"received {operation}")

        if operation == "publish":
            session_id = str(uuid.uuid4())
            self.publish_map[entity_name + target_name] = session_id
        else:
            session_id = self.publish_map[entity_name + target_name]

        post_obj = {'operation': operation, 'entityName': entity_name, 'sessionId': session_id}
        if target_name:
            post_obj['targetName'] = target_name
        if pass_row_index:
            post_obj['rowIndex'] = get_from_thread_store("current_iteration")

        post_response = requests.post(self.settings.get("master_publish_url"), json=post_obj)
        if post_response.status_code >= 400:
            raise TaurusException(
                f"Publish data failed, status code: {post_response.status_code}, reason: {post_response.reason}")
