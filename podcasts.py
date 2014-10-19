#!/usr/bin/env python3

import os
import sys
import urllib3
import json

from xml.dom import minidom as dom
from enum import Enum
from itertools import islice


class Result(Enum):
    Ok = 0
    Err = 1


def validate(url):
    if not url.startswith('http://') or url.count('/') < 3:
        raise RuntimeError(url + ' is not a valid url')


def fileName(url):
    return url.rpartition('/')[-1]


def createFolder(folder):
    if not os.path.exists(folder):
        os.makedirs(folder)
    elif not os.path.isdir(folder):
        raise IOError(folder + " is not a directory")


def fetchFile(hpool, url, filepath):
    """Return: (Result.Ok, path), no problem,
               (Result.Err, url), fetch problem,
               (Result.Err, path), write problem"""
    res, action = Result.Err, url
    try:
        req = hpool.request('GET', url)
        if req.status is 200:
            action = filepath
            with open(filepath, 'wb') as f:
                f.write(req.data)
            res = Result.Ok
    except:
        pass
    return (res, action)


def fetchRecents(hpool, folder, urls, mx=0):
    res = []
    trim_urls = islice(urls, 0, mx) if mx else urls
    for url in trim_urls:
        filename = fileName(url)
        filepath = os.path.join(folder, filename)
        if os.path.exists(filepath):
            break
        else:
            res.append(fetchFile(hpool, url, filepath))
    return res


def downloadRecent(hpool, podinfo, basedir='.', mx=0):
    subfolder, podcast = podinfo
    folder = os.path.join(basedir, subfolder)
    createFolder(folder)
    status, downloaded = Result.Err, []
    try:
        req = hpool.request('GET', podcast)
        if req.status is 200:
            xml = dom.parseString(req.data)
            nodes = xml.getElementsByTagName('enclosure')
            maybe_urls = map((lambda u: u.getAttribute('url')), nodes)
            podcast_urls = filter((lambda u: u), maybe_urls)
            downloaded = fetchRecents(hpool, folder, podcast_urls, mx)
            status = Result.Ok
    except:
        pass
    return (status, downloaded)


class PodGet:
    def __init__(self, settings_fpath, conn=1):
        with open(settings_fpath, 'r') as f:
            self.settings = json.load(f)
        self.hpool = urllib3.PoolManager(conn)

    def download(self):
        basedir = self.settings['folder']
        createFolder(basedir)
        for podinfo in self.settings['podcasts'].items():
            status, downloaded = downloadRecent(self.hpool, podinfo, basedir)
            podcast, _ = podinfo
            print(podcast)
            if status is Result.Err:
                print('\tstatus: err')
            else:
                if downloaded:
                    print('\tlist:', downloaded)
                else:
                    print('\tstatus: No updates')

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("USAGE:", sys.argv[0], "settings_filepath")
        sys.exit(1)
    PodGet(sys.argv[1]).download()
    sys.exit(0)
