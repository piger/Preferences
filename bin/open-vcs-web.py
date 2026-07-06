#!/usr/bin/env python3

import os
import re
import argparse
import subprocess
from urllib.parse import urljoin


def run(cmd: str) -> str:
    return subprocess.getoutput(cmd)

def get_repo_url(filetype: str, filename: str) -> str:
    remote = run("git remote get-url origin")
    branch = run("git rev-parse --abbrev-ref HEAD")

    remote = re.sub(r'\.git$', '', remote)
    remote = remote + "/"
    if "github.com" in remote:
        # github
        tail = os.path.join(filetype, branch, filename)
    else:
        # forgejo, codeberg, etc...
        tail = os.path.join("src", "branch", branch, filename)

    if re.match(r'https?://', remote):
        return urljoin(remote, tail)

    # e.g. git@github.com:piger/Preferences.git
    remote = remote.replace(":", "/", count=1)
    remote = re.sub(r'^[^@]*@?', 'https://', remote)
    return urljoin(remote, tail)

def main():
    parser = argparse.ArgumentParser(
        prog="open-vcs-web",
        description="Open GitHub or Codeberg for the given VCS file",
    )
    parser.add_argument("filename")
    args = parser.parse_args()

    abs_filename = os.path.abspath(args.filename)
    repo_root = run("git rev-parse --show-toplevel")
    rel_filename = os.path.relpath(abs_filename, repo_root)
    filetype = "blob" if not os.path.isdir(abs_filename) else "tree"

    url = get_repo_url(filetype, rel_filename)
    print(url)
    run(f"open {url}")


if __name__ == "__main__":
    main()
