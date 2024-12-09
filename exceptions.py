class EmacsUnavailableException(Exception):
    def __init__(self):
        super().__init__("Have you started the emacs server?")

