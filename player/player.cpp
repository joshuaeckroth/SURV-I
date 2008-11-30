#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <math.h>
#include <iostream>
#include <fstream>
#include <string>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "xmlsp.h"

using namespace std;

Display *display;
Window root;
Window window;
GC gc;
int screen;
unsigned long fgcolor, bgcolor;
Colormap cmap;
Visual *visual;
int depth;
int framenum = 0;
bool quit = 0;

int textColor = 0xffcc66;
int acquisitionColor = 0xcc66ff;
int trackColor = 0x66ffcc;
int noiseColor = 0x432254;
int expectedColor = 0xffcc66;

XGCValues gcValues;
GC trackGc;
GC acquisitionGc;
GC noiseGc;
GC expectedGc;

#define EVENTMASK ExposureMask | KeyPressMask | ButtonPressMask

void xinit(void)
{
    XEvent event;

    display = XOpenDisplay("");
    screen = DefaultScreen(display);
    root = RootWindow(display, screen);

    fgcolor = WhitePixel(display, screen);
    bgcolor = BlackPixel(display, screen);
    visual = DefaultVisual(display, screen);
    depth = DefaultDepth(display, screen);

    cmap = DefaultColormap(display, screen);

    window = XCreateSimpleWindow(display, root,
            0, 0, 640, 480, 0, fgcolor, bgcolor);

    gc = XCreateGC(display, window, 0, 0);

    gcValues.line_width = 3;
    gcValues.cap_style = CapRound;
    gcValues.join_style = JoinRound;

    trackGc = XCreateGC(display, window, (GCLineWidth | GCJoinStyle | GCCapStyle), &gcValues);

    gcValues.line_width = 2;
    acquisitionGc = XCreateGC(display, window, (GCLineWidth | GCJoinStyle | GCCapStyle), &gcValues);

    noiseGc = XCreateGC(display, window, (GCLineWidth | GCJoinStyle | GCCapStyle), &gcValues);

    gcValues.line_width = 2;
    gcValues.line_style = LineOnOffDash;
    expectedGc = XCreateGC(display, window, (GCLineWidth | GCJoinStyle | GCCapStyle | GCLineStyle), &gcValues);

    XSetBackground(display, gc, bgcolor);
    XSetForeground(display, gc, fgcolor);

    XSetFont(display, gc, XLoadFont(display, "-adobe-helvetica-*-r-*-*-*-120-*-*-*-*-*-*"));

    XMapRaised(display, window);

    XSelectInput(display, window, EVENTMASK);
    do {
            XNextEvent(display, &event);
    } while (event.type != Expose);

    XClearWindow(display, window);
}

void xbye(void)
{
    XFreeGC(display, gc);
    XDestroyWindow(display, window);
    XCloseDisplay(display);
}


FILE *video_in, *video_out;
XImage *ximg;


int *read_image(FILE *in, int *width, int *height)
{
    char line[80];
    int *buf;
    int w, h, x, y, c, i;
    
    /* Grab two lines: "P6\nwidth height MAXVAL\n" */
    if (!fgets(line, 80, in)) return 0;
    if (!fgets(line, 80, in)) return 0;
    
    sscanf(line, "%d %d %d", &w, &h, &x);
    
    buf = (int *)malloc(w*h*sizeof(int));
    i = 0;
    if (!buf) return 0;
    
    /* Grab a whole frame */
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            c = getc(in) << 16;
            c |= getc(in) << 8;
            c |= getc(in);
            buf[i++] = c;
        }
    }
    
    *width = w;
    *height = h;
    
    return buf;
}

void write_image(FILE *out, XImage *img, int width, int height)
{
  int x, y;
  long p;
  char r, g, b;

  fprintf(out, "P6\n640 480 255\n");

  for(y = 0; y < height; y++)
    {
      for(x = 0; x < width; x++)
	{
	  p = XGetPixel(img, x, y);
	  r = p >> 16;
	  g = p >> 8;
	  b = p;
	  fprintf(out, "%c%c%c", r, g, b);
	}
    }
}

class MyParser: public XMLSP::Parser
{
public:
    virtual bool on_tag_open(const std::string& tag_name, XMLSP::StringMap& attributes);
    virtual bool on_tag_close(const std::string& tag_name);
    virtual void on_error(int errnr, int line, int col, const std::string& message);
};

int sm_x, sm_y;

void key_wait(void)
{
    XEvent event;
    do {
        XNextEvent(display, &event);
    } while (event.type != KeyPress);
}

void next_frame(void)
{
    int w, h, *buf;
    char framemsg[15];
    
    buf = read_image(video_in, &w, &h);
    if (!buf) return;
    free(buf);
    buf = read_image(video_in, &w, &h);
    if (!buf) return;
    framenum++;
    ximg->data = (char *)buf;
    XPutImage(display, window, gc, ximg, 0, 0, 0, 0, 640, 480);
    ximg->data = 0;
    free(buf);
    sprintf(framemsg, "Frame %d, time %0.2fs", framenum, ((double)framenum) / 3.0);
    XSetForeground(display, gc, textColor);
    XDrawString(display, window, gc, 10, 470, framemsg, strlen(framemsg));
    XFlush(display);
}

string line;

void MyParser::on_error(int errnr, int line, int col, const std::string& message)
{
    cerr << "player: parser error (line " << line << " col " << col << "): " << message << endl;
}

bool MyParser::on_tag_close(const std::string& tag_name)
{
    if (tag_name == "Frame") {
      //key_wait();
      if(video_out != NULL)
	{
	  write_image(video_out, XGetImage(display, window, 0, 0, 640, 480, AllPlanes, XYPixmap), 640, 480);
	}
      else
	{
	  usleep(200000);
	  key_wait();
	}
    }
    else if (tag_name == "WorldEvents") { // done with video
      key_wait();
      quit = true;
    }
    return true;
}

bool MyParser::on_tag_open(const std::string& tag_name, XMLSP::StringMap& attributes)
{
    string type;
    int this_framenum = 0;
    int x, y, ox, oy, ex, ey, w, h, radius;
    const char *id, *prevID, *thisFrame;

    if (tag_name == "WorldEvents") {
      next_frame(); // show initial frame
    } else if (tag_name == "Frame") {
      this_framenum = atoi(attributes["framenum"].c_str());

      while (this_framenum > framenum) {
	next_frame();
      }
    }

    if (tag_name == "Acquisition") {
        x = atoi(attributes["x"].c_str());
        y = atoi(attributes["y"].c_str());
        w = atoi(attributes["width"].c_str());
        h = atoi(attributes["height"].c_str());
        id = attributes["id"].c_str();

        XSetForeground(display, acquisitionGc, acquisitionColor);
        XDrawString(display, window, gc, x-(w/2)-5, y-(h/2)-5, id, strlen(id));
	XDrawRectangle(display, window, acquisitionGc, x-(w/2), y-(h/2), w, h);
    }
    else if(tag_name == "Noise") {
      x = atoi(attributes["x"].c_str());
      y = atoi(attributes["y"].c_str());
      w = atoi(attributes["width"].c_str());
      h = atoi(attributes["height"].c_str());
      id = attributes["id"].c_str();

      // Draw noise box (w*h box with X through it) and a circle around center
      XSetForeground(display, noiseGc, noiseColor);
      XDrawRectangle(display, window, noiseGc, x-(w/2), y-(h/2), w, h);
      XDrawLine(display, window, noiseGc, x-(w/2), y-(h/2), x-(w/2)+w, y-(h/2)+h);
      XDrawLine(display, window, noiseGc, x-(w/2), y-(h/2)+h, x-(w/2)+w, y-(h/2));
      XDrawArc(display, window, noiseGc, x-10, y-10, 20, 20, 0, 360*64);
    }
    else if(tag_name == "Track") {
      x = atoi(attributes["x"].c_str());
      y = atoi(attributes["y"].c_str());
      ox = atoi(attributes["ox"].c_str());
      oy = atoi(attributes["oy"].c_str());
      ex = atoi(attributes["ex"].c_str());
      ey = atoi(attributes["ey"].c_str());
      id = attributes["id"].c_str();
      prevID = attributes["prevID"].c_str();
      radius = atoi(attributes["radius"].c_str());
      thisFrame = attributes["thisFrame"].c_str();

      // Draw track segment
      XSetForeground(display, trackGc, trackColor);
      XDrawLine(display, window, trackGc, ox, oy, x, y);

      // Only draw extra info if the track is current (this frame)
      if(strncmp(thisFrame, "True", 4) == 0)
	{
	  // Draw track ID
	  XDrawString(display, window, gc, x+5, y+5, id, strlen(id));
	  
	  // Draw expected location
	  XSetForeground(display, expectedGc, expectedColor);
	  XDrawLine(display, window, expectedGc, x, y, ex, ey);

	  // Draw radius
	  XDrawArc(display, window, expectedGc, ex - radius, ey - radius, 2 * radius, 2 * radius, 0, 360*64);
	}
    }

      /*
        } else if (type == "ambiguity") {
            // Draw ambiguity box (w*h box with X through it) and a circle around center
            XSetForeground(display, gc, 0xbbbbbb);
            XDrawRectangle(display, window, gc, x-(w/2), y-(h/2), w, h);
            XDrawLine(display, window, gc, x-(w/2), y-(h/2), x-(w/2)+w, y-(h/2)+h);
            XDrawLine(display, window, gc, x-(w/2), y-(h/2)+h, x-(w/2)+w, y-(h/2));
            XDrawArc(display, window, gc, x-10, y-10, 20, 20, 0, 360*64);
        } else if (type == "person") {
            // Draw box around person
            XSetForeground(display, gc, 0xa8f794);
            XDrawRectangle(display, window, gc, x-(w/2), y-(h/2), w, h);
        } else if (type == "vehicle") {
            // Draw box around vehicle
            XSetForeground(display, gc, 0x94daf7);
            XDrawRectangle(display, window, gc, x-(w/2), y-(h/2), w, h);
        } else if (type == "people") {
            // Draw box around people
            XSetForeground(display, gc, 0xe494f7);
            XDrawRectangle(display, window, gc, x-(w/2), y-(h/2), w, h);
        }
        else if (type == "split" || type == "merge" || type == "ambiguity") {
            sm_x = atoi(attributes["x"].c_str());
            sm_y = atoi(attributes["y"].c_str());
            // Draw circle
            if (type == "split") {
                XSetForeground(display, gc, 0xff0000);
            } else if (type == "ambiguity") {
                XSetForeground(display, gc, 0x7f007f);
            } else {
                XSetForeground(display, gc, 0x00ff00);
            }
            XDrawArc(display, window, gc, sm_x-10, sm_y-10, 20, 20, 0, 360*64);
        } else if (type == "new") {
            x = atoi(attributes["x"].c_str());
            y = atoi(attributes["y"].c_str());
            // Draw object id
            XSetForeground(display, gc, 0xa8f794);
            obj_id_str = attributes["obj"];
            XDrawString(display, window, gc, x-10, y-10, obj_id_str.c_str(), obj_id_str.length());
            // Draw circle
            XSetForeground(display, gc, 0x000000);
            XDrawArc(display, window, gc, x-10, y-10, 20, 20, 0, 360*64);
            // Draw track
            track_width = atoi(attributes["track_width"].c_str());
            track_height = atoi(attributes["track_height"].c_str());
            XSetForeground(display, gc, 0x888888);
            XDrawRectangle(display, window, gc, x-(track_width/2), y-(track_height/2), track_width, track_height);
            //cerr << "player: New object " << attributes["obj"] << endl;
        } else if (type == "move") {
            x = atoi(attributes["x"].c_str());
            y = atoi(attributes["y"].c_str());
            ox = atoi(attributes["ox"].c_str());
            oy = atoi(attributes["oy"].c_str());
            // Draw object ids
            XSetForeground(display, gc, 0xa8f794);
            obj_id_str = attributes["old"] + "->" + attributes["new"];
            XDrawString(display, window, gc, x-10, y-10, obj_id_str.c_str(), obj_id_str.length());
            // Draw circles and line
            XSetForeground(display, gc, 0x00ff00);
            XDrawArc(display, window, gc, x-10, y-10, 20, 20, 0, 360*64);
            XSetForeground(display, gc, 0xff0000);
            XDrawArc(display, window, gc, ox-10, oy-10, 20, 20, 0, 360*64);
            XDrawLine(display, window, gc, x, y, ox, oy);
            // Draw track
            track_width = atoi(attributes["track_width"].c_str());
            track_height = atoi(attributes["track_height"].c_str());
            otrack_width = atoi(attributes["otrack_width"].c_str());
            otrack_height = atoi(attributes["otrack_height"].c_str());
            XSetForeground(display, gc, 0x555555);
            XDrawRectangle(display, window, gc, x-(track_width/2), y-(track_height/2), track_width, track_height);
            XSetForeground(display, gc, 0x888888);
            XDrawRectangle(display, window, gc, ox-(otrack_width/2), oy-(otrack_height/2), otrack_width, otrack_height);
            //cerr << "player: Object " << attributes["old"] << " moved from "
                << ox << "," << oy << " to " << x << "," << y 
                << " (new object id: " << attributes["new"] << ")" << endl;
        }
    } else if (tag_name == "obj") {
        //cerr << "player: Split target " << attributes["obj"] << endl;
        x = atoi(attributes["x"].c_str());
        y = atoi(attributes["y"].c_str());
        // Draw circle and line
        XSetForeground(display, gc, 0x007f7f);
        XDrawArc(display, window, gc, x-10, y-10, 20, 20, 0, 360*64);
        XDrawLine(display, window, gc, x, y, sm_x, sm_y);
    } else if (tag_name == "parent") {
        //cerr << "player: Merge parent " << attributes["obj"] << endl;
        x = atoi(attributes["x"].c_str());
        y = atoi(attributes["y"].c_str());
        // Draw circle and line
        XSetForeground(display, gc, 0xff0000);
        XDrawArc(display, window, gc, x-10, y-10, 20, 20, 0, 360*64);
        XDrawLine(display, window, gc, x, y, sm_x, sm_y);
    }
    */

    XFlush(display);

    return true;
}

char command[400];

int main(int argc, char *argv[])
{
    MyParser parser;
    int n;
    string line;

    if (argc<2) {
      printf("Usage: %s <video_in.ppm> [video_out.ppm]\n", argv[0]);
      exit(0);
    }

    xinit();

    ximg = XCreateImage(display, visual, depth, ZPixmap, 0, 0, 640, 480, 32, 0);
    
    video_in = fopen(argv[1], "r");
    if(argc == 3)
      {
	video_out = fopen(argv[2], "w");
      }
    else
      {
	video_out = NULL;
      }

    parser.begin();
    while(!cin.eof() && !quit)
    {
      getline(cin, line);
      if (parser.parse_chunk(line) == false) {
	cerr << "player: Parser error:" << endl;
	cerr << "player: " << line << endl;
	exit(0);
      }
    }
    parser.end();
    xbye();
    return 0;
}
