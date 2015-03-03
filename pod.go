package main

import (
    "os"
    "fmt"
    "strings"
    "net/http"
    "io/ioutil"
    "encoding/xml"
    "encoding/json"
)

type RSS struct {
    XMLName xml.Name `xml:"rss"`
    Items Items `xml:"channel"`
}

type Items struct {
    XMLName xml.Name `xml:"channel"`
    ItemList []Item `xml:"item"`
}

type Item struct {
    Title string `xml:"title"`
    Link string `xml:"link"`
    Description string `xml:"description"`
    Enclosure Enclosure `xml:"enclosure"`
}

type Enclosure struct {
    Url string `xml:"url,attr"`
}

type Podcast struct {
    Folder string `json:"folder"`
    Url   string `json:"url"`
}

type Json struct {
    Folder string `json:"folder"`
    Podcasts []Podcast `json:"podcasts"`
}

type Podchan struct {
    base string
    data chan Podcast
    done chan bool
}

func urlToLocalPath(base string, pod Podcast) string {
    split := strings.SplitAfter(pod.Url, "/")
    audioFile := split[len(split)-1]
    audioPath := base + "/" + pod.Folder + "/" + audioFile
    return audioPath
}

func getAudioSources(base string, setting Podcast, res chan Podcast,
status chan string) {
    resp, err := http.Get(setting.Url)
    if err != nil {
        status <- "error getting: " + setting.Url
        return
    }
    defer resp.Body.Close()
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }
    var feed RSS
    err = xml.Unmarshal([]byte(body), &feed)
    if err != nil {
        status <- "error parsing: " + setting.Url
        return
    }
    for _, item := range feed.Items.ItemList {
        pod := Podcast{setting.Folder, item.Enclosure.Url}
        audioPath := urlToLocalPath(base, pod)
        _, err := os.Stat(audioPath)
        if err == nil {
            return
        } else if os.IsNotExist(err) {
            res <-pod
        } else {
            status <- "error permissions: " + audioPath
            return
        }
    }
}

func parseRSS(pods Podchan, data chan Podcast, done chan bool,
status chan string) {
    for {
        select {
        case msg := <-pods.data:
            getAudioSources(pods.base, msg, data, status)
        case <-pods.done:
            done <-true
            return
        }
    }
}

func saveFile(base string, pod Podcast, status chan string) {
    path := urlToLocalPath(base, pod)
    fmt.Println(path)
    file, err := os.Create(path)
    if err != nil {
        panic(err)
    }
    defer file.Close()
    res, err := http.Get(pod.Url)
    if err != nil {
        status <-"error fetching: " + pod.Url
    }
    defer res.Body.Close()
    content, err := ioutil.ReadAll(res.Body)
    if err != nil {
        panic(err)
    }
    _, err = file.Write(content)
    if err != nil {
        panic(err)
    }
}

func downloadPods(files Podchan, done chan bool, status chan string) {
    for {
        select {
        case msg := <-files.data:
            status <-"getting: " + msg.Url
             saveFile(files.base, msg, status)
        case <-files.done:
            done <-true
            return
        }
    }
}

func readSettings(path string) Json {
    file, err := os.Open(path)
    if err != nil {
      panic(err)
    }
    defer file.Close()
    content, err := ioutil.ReadAll(file)
    if err != nil {
        panic(err)
    }
    var settings Json
    err = json.Unmarshal([]byte(content), &settings)
    if err != nil {
        panic(err)
    }
    return settings
}

func newPodchan(settings Json, max int) Podchan {
    return Podchan{ settings.Folder, make(chan Podcast), make(chan bool, max) }
}

func createDirs(settings Json) {
    for _, item := range settings.Podcasts {
        os.Mkdir(settings.Folder + "/" + item.Folder, 0666)
    }
}

func displayErrors(err chan string) {
    for {
        fmt.Println(<-err)
    }
}

const max = 8

/*
first go program, fatal errors, poor structure, just toying
setting schema:
{"podcasts": [{"folder": "", "url": ""},], "folder": "/home/user/localdir"}
TODO: create local dirs, show err/status \ fatal , bufio
*/

func main() {
    if len(os.Args) != 2 {
        panic("USAGE: " + os.Args[0] + " settings.json")
    }
    settings := readSettings(os.Args[1])
    createDirs(settings)
    ioUrls := newPodchan(settings, max)
    ioFiles := newPodchan(settings, max)
    doneFetching := make(chan bool, max)
    readyFiles := make(chan bool, max)
    status := make(chan string)

    go displayErrors(status)
    for i := 0; i < max; i++ {
        go parseRSS(ioUrls, ioFiles.data, doneFetching, status)
        go downloadPods(ioFiles, readyFiles, status)
    }
    for _, pod := range settings.Podcasts {
        ioUrls.data <-pod
    }
    for i := 0; i < max; i++ {
        ioUrls.done <-true
        <-doneFetching
        ioFiles.done <-true
    }
    for i := 0; i < max; i++ {
        <-readyFiles
    }
}


