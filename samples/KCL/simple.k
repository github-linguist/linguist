schema App:
    """The application model."""
    name: str
    replicas: int
    labels?: {str:str} = {app = name}

app: App {
    name = "app"
    replicas = 1
    labels.key = "value"
}
