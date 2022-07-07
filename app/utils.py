from pydantic import BaseModel


supported_langs_dict = {
    "Assembly": [".asm"],
    "Batchfile": [".bat", ".cmd"],
    "C": [".c", ".h"],
    "C#": [".cs"],
    "C++": [".cpp", ".hpp", ".c++", ".h++", ".cc", ".hh", ".C", ".H"],
    "CMake": [".cmake"],
    "CSS": [".css"],
    "Dockerfile": [".dockerfile", "Dockerfile"],
    "FORTRAN": [".f90", ".f", ".f03", ".f08", ".f77", ".f95", ".for", ".fpp"],
    "GO": [".go"],
    "Haskell": [".hs"],
    "HTML": [".html"],
    "Java": [".java"],
    "JavaScript": [".js"],
    "Julia": [".jl"],
    "Lua": [".lua"],
    "Makefile": ["Makefile"],
    "Markdown": [".md", ".markdown"],
    "PHP": [".php", ".php3", ".php4", ".php5", ".phps", ".phpt"],
    "Perl": [".pl", ".pm", ".pod", ".perl"],
    "PowerShell": [".ps1", ".psd1", ".psm1"],
    "Python": [".py"],
    "Ruby": [".rb"],
    "Rust": [".rs"],
    "SQL": [".sql"],
    "Scala": [".scala"],
    "Shell": [".sh", ".bash", ".command", ".zsh"],
    "TypeScript": [".ts", ".tsx"],
    "TeX": [".tex"],
    "Visual Basic": [".vb"],
}


lang_mappings = {
    0: "Assembly",
    1: "Batchfile",
    2: "C",
    3: "C#",
    4: "C++",
    5: "CMake",
    6: "CSS",
    7: "Dockerfile",
    8: "FORTRAN",
    9: "GO",
    10: "HTML",
    11: "Haskell",
    12: "Java",
    13: "JavaScript",
    14: "Julia",
    15: "Lua",
    16: "Makefile",
    17: "Markdown",
    18: "PHP",
    19: "Perl",
    20: "PowerShell",
    21: "Python",
    22: "Ruby",
    23: "Rust",
    24: "SQL",
    25: "Scala",
    26: "Shell",
    27: "TeX",
    28: "TypeScript",
    29: "Visual Basic",
}


class Features(BaseModel):
    """Features model to be used for prediction"""

    code_snippet: str


class Prediction(BaseModel):
    """Prediction result model"""

    predicted_class: int
    predicted_language: str
    predicted_value: float
