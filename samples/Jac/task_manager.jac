import from byllm.llm { Model }
import time;

glob llm = Model(model_name="gpt-4o");

node Task {
    has task:str = "";
    has date:str = "";
    has time:str = "";
}

node TaskHandling {
    def get_current_time() -> str {
        return time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime());
    }
    def add_task(task: str, date: str, time: str) -> str {
        task_created = Task(task=task, date=date, time=time);
        self ++> task_created;
        return "Task added successfully";
    }
    def summarize_tasks -> str {
        scheduled_tasks = [self-->(`?Task)];
        return str(scheduled_tasks);
    }
    def route_and_run(utterance: str) -> str by llm(
        method="ReAct",
        tools=([self.add_task, self.get_current_time, self.summarize_tasks])
    );
    can execute with task_manager entry {
        print("[TaskHandling Node Activated]");
        response = self.route_and_run(visitor.utterance);
        print("→", response);
        report {
            "utterance": visitor.utterance,
            "response": response,
            "node_type": self.__class__.__name__
        };
    }
}

node EmailHandling {
    def write_email_content(utterance: str) -> str by llm();
    def route_and_run(utterance: str) -> str by llm(
        method="ReAct",
        tools=([self.write_email_content])
    );
    can execute with task_manager entry {
        print("[EmailHandling Node Activated]");
        response = self.route_and_run(visitor.utterance);
        print("→", response);
        report {
            "utterance": visitor.utterance,
            "response": response,
            "node_type": self.__class__.__name__
        };
    }
}

node GeneralChat {
    def chat(utterance: str) -> str by llm();
    can execute with task_manager entry {
        print("[GeneralChat Node Activated]");
        response = self.chat(visitor.utterance);
        print("→", response);
        report {
            "utterance": visitor.utterance,
            "response": response,
            "node_type": self.__class__.__name__
        };
    }
}

enum RoutingNodes{
    TASK_HANDLING,
    EMAIL_HANDLING,
    GENERAL_CHAT
}

walker task_manager {
    has utterance: str = "";

    def route_to_node(utterance: str) -> RoutingNodes by llm();
    can execute with `root entry {
        routed_node_name = self.route_to_node(self.utterance);

        node_map = {
            RoutingNodes.TASK_HANDLING: TaskHandling,
            RoutingNodes.EMAIL_HANDLING: EmailHandling,
            RoutingNodes.GENERAL_CHAT: GeneralChat
        };
        node_type = node_map[routed_node_name];
        routed_node = [-->(`?node_type)];
        if not routed_node {
            routed_node = here ++> node_type();
        }
        visit routed_node[0];
    }
}