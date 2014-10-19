#!/usr/bin/env python3

import urllib3
import os
import json
from xml.dom import minidom
from enum import Enum

setting_file = 'podcasts.json'


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
    """(err, url) is a fetch problem,
       (err, path) is a permission problem"""
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
    trim_urls = zip(urls, range(mx)) if mx else zip(urls, enumerate(urls))
    for url, _ in trim_urls:
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
            content_xml = minidom.parseString(req.data)
            nodes = content_xml.getElementsByTagName('enclosure')
            podcast_urls = [
                u.getAttribute('url') for u in nodes if u.getAttribute('url')
            ]
            downloaded = fetchRecents(hpool, folder, podcast_urls, mx)
            status = Result.Ok
    except:
        pass
    return (status, downloaded)


class PodGet:
    def __init__(self):
        with open(setting_file, 'r') as f:
            self.settings = json.load(f)
        self.hpool = urllib3.PoolManager(8)

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
    PodGet().download()
