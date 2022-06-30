from pydantic import BaseModel


supported_langs_dict = {
    "Assembly": [".asm"],
    # "Batchfile": [".bat", ".cmd"],
    "C": [".c", ".h"],
    "C#": [".cs"],
    "C++": [".cpp", ".hpp", ".c++", ".h++", ".cc", ".hh", ".C", ".H"],
    # "CMake": [".cmake"],
    # "CSS": [".css"],
    # "Dockerfile": [".dockerfile", "Dockerfile"],
    # "FORTRAN": [".f90", ".f", ".f03", ".f08", ".f77", ".f95", ".for", ".fpp"],
    "GO": [".go"],
    # "Haskell": [".hs"],
    # "HTML": [".html"],
    "Java": [".java"],
    "JavaScript": [".js"],
    "Julia": [".jl"],
    "Lua": [".lua"],
    # "Makefile": ["Makefile"],
    # "Markdown": [".md", ".markdown"],
    # "PHP": [".php", ".php3", ".php4", ".php5", ".phps", ".phpt"],
    # "Perl": [".pl", ".pm", ".pod", ".perl"],
    # "PowerShell": [".ps1", ".psd1", ".psm1"],
    "Python": [".py"],
    "Ruby": [".rb"],
    "Rust": [".rs"],
    "SQL": [".sql"],
    # "Scala": [".scala"],
    # "Shell": [".sh", ".bash", ".command", ".zsh"],
    # "TypeScript": [".ts", ".tsx"],
    # "TeX": [".tex"],
    "Visual Basic": [".vb"],
}


lang_mappings = {
    0: "Assembly",
    1: "C",
    2: "C#",
    3: "C++",
    4: "GO",
    5: "Java",
    6: "JavaScript",
    7: "Julia",
    8: "Lua",
    9: "Python",
    10: "Ruby",
    11: "Rust",
    12: "SQL",
    13: "Visual Basic",
}


class Features(BaseModel):
    """Features model to be used for prediction"""

    code_snippet: str


class Prediction(BaseModel):
    """Prediction result model"""

    predicted_class: int
    predicted_language: str
    predicted_value: float
