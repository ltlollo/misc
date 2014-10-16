#!/usr/bin/env python3


import os
import json

from xml.dom import minidom

SCHEME_DICT = {'TEL': 'tel:',
               'EMAIL': 'mailto:',
               'X-MSN': 'mailto:',
               'X-GOOGLE-TALK': 'mailto:',
               'URL': ''}

MODE_HTML = 'html'
MODE_PLAIN = 'plain'
MODE_URL_NOPREFIX = 'url'
MODE_VANILLA = None


illegal_charset = '<>:"/\|?*'

t_table = str.maketrans('', '', illegal_charset)


def sanitize(name):
    """Removes the /illegal/ characters from the (name) string."""
    sane_name = name.translate(t_table)
    return sane_name


def vcard_list(DOMnodes):
    return map(lambda x: x.firstChild.toxml().split('\n'), DOMnodes)


def write_bookmark(path, name, url, mode):
    """Creates a (name + extension) file in (path/) with (url) as content,
    if file exists, uses (_{1,n} + file) instead. If mode is MODE_HTML the
    content is a link pointing to (url).
    """
    if mode is not MODE_VANILLA:
        extension = '.html'
    else:
        extension = '.uri'

    sane_name = sanitize(name) + extension

    # bookmark names are not guaranteed to be unique.
    while(os.path.exists(sane_name)):
        sane_name = ''.join(['_', sane_name])

    bookmark = open(os.path.join(path, sane_name), 'w')
    field = gen_field('link', 'URL', url, mode)
    bookmark.write(field + '\n')
    bookmark.close()


def populate_directory_tree(dtree, parent, mode):
    """Populates the (parent) node directory with the bookmark tree view,
    given that (parent) as a direcory does _NOT_ exist.

    Parameters
    ----------
    dtree: dict
        The current tree node originally parsed by json.load.
    parent: str
        The current position in the tree view.
    mode: MODE (str)
        Use MODE_HTML to get pseudo html files
    """
    if 'children' in dtree:
        # then we are in an internal node, aka a directory.
        dir_name = os.path.join(parent, sanitize(dtree['title']))
        os.mkdir(dir_name)
        # dtree['children'] is a list of sub-nodes.
        for ele in dtree['children']:
            populate_directory_tree(ele, dir_name, mode)
    else:
        # then we are in a leaf, aka a bookmark.
        write_bookmark(parent, dtree['title'], dtree['uri'], mode)


def gen_field(tag, supported_tag, data, mode):
    """Constructs the hyperlink according to (mode) from tag and data.

    Parameters
    ----------
    tag: str
        The full scheme found in the vcard that matches a supported_tag.
        eg: EMAIL;HOME
    supported_tag: str
        A scheme key in SCHEME_DICT. eg: EMAIL
    data: str
        The data associated to (tag). eg: john@spam.net
    mode: MODE (str)
    """
    if data:
        if mode is MODE_HTML:
            field = ''.join(['<h1><a href="',
                             SCHEME_DICT[supported_tag], data, '">',
                             tag, ": ", data,
                             '</a></h1>'])
        elif mode is MODE_PLAIN:
            field = ''.join([tag, ': ', data])
        elif mode is MODE_URL_NOPREFIX:
            field = ''.join(['<h1><a href="',
                             SCHEME_DICT[supported_tag], data, '">',
                             data, '</a></h1>'])
        else:
            field = data
        return field


def contact_from_vcard(vcard, mode):
    """Returns a tuple (name, contact_info), where name is the name of the
    contact and contact_info is a list attributes if the attributes are
    supported. When no name or valid tags are found, None is returned.
    Valid tags are listed in SCHEME_DICT.

    Parameters
    ----------
    vcard: list
        A list of unicode strings possibly u(SCHEME + u':' + DATA)
    """
    name, contact_info = None, []
    for line in vcard:
        # we want a SCHEME:DATA pattern, but not the photo garbage
        if ':' in line and 'PHOTO' not in line:
            # then we have a possibly interesting tag
            tag, data = line.split(':', 1)
            for supported_tag in SCHEME_DICT:
                if supported_tag in tag:
                    field = gen_field(tag, supported_tag, data, mode)
                    contact_info.append(field)
                    break
            if 'FN' in tag:
                name = data
    if name and contact_info:
        return (name, contact_info)


def write_contact(path, vcard, mode):
    """Creates a (name + extension) file in (path/) with SCHEME_DICT tags
    as content where found. If file exists, uses (_{1,n} + file) instead.
    If mode is MODE_HTML the content is a link pointing to the appropriate
    tag, with the appropriate scheme name.
    """
    contact = contact_from_vcard(vcard, mode)
    if contact:
        if mode is not MODE_VANILLA:
            extension = '.html'
        else:
            extension = '.ucn'

        name, fields = contact
        sane_name = sanitize(name) + extension
        # names are not guaranteed to be unique
        while(os.path.exists(sane_name)):
            sane_name = ''.join(['_', sane_name])
            # I hope you are happy now, u'__John Smith'

        contact_file = open(os.path.join(path, sane_name), 'w')
        for field in fields:
            contact_file.write(field + '\n')
        contact_file.close()


def export_bookmarks(filename, backup_rootdir, mode=None):
    if os.path.isfile(filename):
        json_file = open(filename, 'r')
        json_obj = json.load(json_file)
        if not os.path.exists(backup_rootdir):
            populate_directory_tree(json_obj, backup_rootdir, mode)
        else:
            print('Error: "', backup_rootdir, '" directory already exists.')
    else:
        print(filename, " not found.")


def export_contacts(filename, backup_rootdir, mode=None):
    if os.path.isfile(filename):
        cxml = minidom.parse(filename)
        dom_list = cxml.getElementsByTagName('contact')
        if not os.path.exists(backup_rootdir):
            os.mkdir(backup_rootdir)
            for vcard in vcard_list(dom_list):
                write_contact(backup_rootdir, vcard, mode)
        else:
            print('Error: "', backup_rootdir, '" directory already exists.')
    else:
        print(filename, " not found.")


# export_bookmarks('bookmarks-2013-01-01.json', 'book', mode=MODE_URL_NOPREFIX)
# export_bookmarks('bookmarks-2013-01-01.json', 'book')
# export_contacts('contacts_export_20130101.xml', 'con', mode=MODE_HTML)
