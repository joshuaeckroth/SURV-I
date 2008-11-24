#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <math.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

Display *display;
Window root;
Window window;
GC gc;
int screen;
unsigned long fgcolor, bgcolor;
Colormap cmap;
Visual *visual;
int depth;
char msg[1024]; /* output through socket */
int servfd, clifd; /* server & client socket file descriptors */
int framenum = 0; /* current frame number; incremented by read_image() */

#define EVENTMASK ExposureMask | KeyPressMask | ButtonPressMask

void xinit(void)
{
        XEvent event;
        XColor col;

        display = XOpenDisplay("");
        screen = DefaultScreen(display);
        root = RootWindow(display, screen);

        fgcolor = WhitePixel(display, screen);
        bgcolor = BlackPixel(display, screen);
        visual = DefaultVisual(display, screen);
        depth = DefaultDepth(display, screen);

        cmap = DefaultColormap(display, screen);

        window = XCreateSimpleWindow(display, root,
                0, 0, 1280, 480, 0, fgcolor, bgcolor);

        gc = XCreateGC(display, window, 0, 0);
        XSetBackground(display, gc, bgcolor);
        XSetForeground(display, gc, fgcolor);

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


void socket_init(const char *name)
{
    int clilen, servlen;
    struct sockaddr_un cli_addr, serv_addr;
    
    if((servfd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
        perror("decode: Error opening socket");
        exit(1);
    }
    memset(&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sun_family = AF_UNIX;
    strcpy(serv_addr.sun_path, name);
    servlen = strlen(serv_addr.sun_path) + sizeof(serv_addr.sun_family);
    if(bind(servfd, (struct sockaddr *)&serv_addr, servlen) < 0)
    {
        perror("decode: Error binding to socket");
        exit(1);
    }
    fprintf(stderr, "decode: got layer-1 socket\n");
    listen(servfd, 1);
    
    clilen = sizeof(cli_addr);
    clifd = accept(servfd, (struct sockaddr *)&cli_addr, &clilen);
    if(clifd < 0)
    {
        perror("decode: Error accepting client");
        exit(1);
    }
    fprintf(stderr, "decode: got layer-1 client\n");
}

void socket_close(void)
{

}

void get_ok(void)
{
    char ack[3];
    fprintf(stderr, "decode: WAITING FOR OK\n");
    while((0 == read(clifd, ack, 3)) || (0 != strncmp(ack, "OK", 2)));
    fprintf(stderr, "decode: GOT OK\n");
}

void get_next(void)
{
    char ack[5];
    fprintf(stderr, "decode: WAITING FOR NEXT\n");
    while((0 == read(clifd, ack, 5)) || (0 != strncmp(ack, "NEXT", 4)));
    fprintf(stderr, "decode: GOT NEXT\n");
}

int *read_image(FILE *in, int *width, int *height)
{
    char line[80];
    int *buf;
    int w, h, x, y, c, i;
    
    /* Grab two lines */
    if (!fgets(line, 80, in)) return 0;
    if (!fgets(line, 80, in)) return 0;

    /* PPM format: "width heigh MAXVAL" */
    sscanf(line, "%d %d %d", &w, &h, &x);
    
    buf = malloc(w*h*sizeof(int));
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


void write_image(FILE *out, int *image, int w, int h)
{
    int i, n;
    fprintf(out, "P6\n%d %d 255\n", w, h);
    
    n = w*h;
    for (i=0; i<n; i++) {
        putc(*image >> 16, out);
        putc(*image >> 8, out);
        putc(*image, out);
        
        image++;
    }
}





void blur(int *f1, int *f2, int w, int h)
{
    int x, y, i, c0, c1;
    
    i = 0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            c0 = 0;
            c1 = 0;
            if (x>0) {
                c0++;
                c1 += f1[i-1];
            }
            if (x<w-1) {
                c0++;
                c1 += f1[i+1];
            }
            if (y>0) {
                c0++;
                c1 += f1[i-w];
            }
            if (y<h-1) {
                c0++;
                c1 += f1[i+w];
            }
            
            c1 /= c0-0.1;
            
            f2[i] = c1;
            i++;
        }
    }
}


int *color_blur(int *f1, int w, int h)
{
    int x, y, i, r, g, b, c;
    int *f2 = malloc(w*h*sizeof(int));
    
    i = 0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            c = 0;
            r = 0;
            g = 0;
            b = 0;
            if (x>0) {
                c++;
                b += f1[i-1] & 255;
                g += (f1[i-1] >> 8) & 255;
                r += (f1[i-1] >> 16) & 255;
            }
            if (x<w-1) {
                c++;
                b += f1[i+1] & 255;
                g += (f1[i+1] >> 8) & 255;
                r += (f1[i+1] >> 16) & 255;
            }
            if (y>0) {
                c++;
                b += f1[i-w] & 255;
                g += (f1[i-w] >> 8) & 255;
                r += (f1[i-w] >> 16) & 255;
            }
            if (y<h-1) {
                c++;
                b += f1[i+w] & 255;
                g += (f1[i+w] >> 8) & 255;
                r += (f1[i+w] >> 16) & 255;
            }
            c++;
            b += f1[i] & 255;
            g += (f1[i] >> 8) & 255;
            r += (f1[i] >> 16) & 255;
            
            r /= c;
            g /= c;
            b /= c;
            
            /* if (r>255) r = 255; */
            /* if (g>255) g = 255; */
            /* if (b>255) b = 255; */
            
            f2[i] = (r << 16) | (g << 8) | b;
            i++;
        }
    }
    
    free(f1);
    return f2;
}

double compare(int *f1, int *f2, int x1, int y1, int x2, int y2, int w, int h)
{
    int sc=0, xo, yo, xa, ya, xb, yb, d, p1, p2;
    int se=0, xo1, xo2, yo1, yo2;
    unsigned char *pa, *pb;
    
    yo1 = -7;
    if (y1+yo1 < 0) yo1 = -y1;
    if (y2+yo1 < 0) yo1 = -y2;
    yo2 = 7;
    if (y1+yo2 >= h) yo2 = h-1-y1;
    if (y2+yo2 >= h) yo2 = h-1-y2;
    
    ya = y1+yo1;
    yb = y2+yo1;

    for (yo=yo1; yo<=yo2; yo++) {
        pa = f1 + ya*w;
        pb = f2 + yb*w;
        
        xo1 = -7;
        if (x1+xo1 < 0) xo1 = -x1;
        if (x2+xo1 < 0) xo1 = -x2;
        xo2 = 7;
        if (x1+xo2 >= w) xo2 = w-1-x1;
        if (x2+xo2 >= w) xo2 = w-1-x2;
        
        xa = x1+xo1;
        xb = x2+xo1;
        pa += xa*4;
        pb += xb*4;
        
        for (xo=xo1; xo<=xo2; xo++) {
            /*p1 = *pa++;
            p2 = *pb++;
            d = (int)(unsigned char)p1 - (int)(unsigned char)p2;
            se += d*d;
            d = (int)(unsigned char)(p1 >> 8) - (int)(unsigned char)(p2 >> 8);
            se += d*d;
            d = (p1 >> 16) - (p2 >> 16);
            se += d*d;*/
            
            d = (int)*pa++ - (int)*pb++;
            se += d*d;
            d = (int)*pa++ - (int)*pb++;
            se += d*d;
            d = (int)*pa++ - (int)*pb++;
            se += d*d;
            
            pa++;
            pb++;
        }
        
        sc += xo2-xo1+1;
        
        ya++;
        yb++;
    }
    
    if (sc < 4) return 1000000;
    
    return sqrt((double)se / (double)sc);
}


void motion(int *f1, int *f2, int *mx, int *my, int w, int h)
{
    int x, y, wx, wy, x2, y2;
    double s, best, worst;
    int bx, by;
    double err[33][33];
    
    for (y=0; y<h; y++) {
        /* printf("%d\n", y); */
        for (x=0; x<w; x++) {
            best = 1000;
            worst = -1000;
            bx = x;
            by = y;
            
            for (wy=0; wy<33; wy++) {
                for (wx=0; wx<33; wx++) {
                    err[wy][wx] = -1;
                }
            }
            err[16][16] = compare(f1, f2, x, y, x, y, w, h);
            best = err[16][16];
            
            wy = 0;
            wx = 0;
    
            
            do {
                /* printf("%d %d\n", wx, wy); */
                bx = 0;
                by = 0;
                
                for (y2=-1; y2<=1; y2++) {
                    if (wy+y2<-16) continue;
                    if (wy+y2>16) continue;
                    for (x2=-1; x2<=1; x2++) {
                        if (wx+x2<-16) continue;
                        if (wx+x2>16) continue;
                        if (err[16+wy+y2][16+wx+x2] < 0) {
                            /* printf("  %d %d\n", x2, y2); */
                            s = compare(f1, f2, x, y, x+wx+x2, y+wy+y2, w, h);
                            err[16+wy+y2][16+wx+x2] = s;
                            if (s < best) {
                                best = s;
                                bx = x2;
                                by = y2;
                            }
                        }
                    }
                }
                
                wx += bx;
                wy += by;
                /* if (bx || by) printf("%d %d\n", bx, by); */
            } while (bx || by);
            /* printf("\n"); */

            bx = x+wx;
            by = y+wy;
            
            /* if (worst - best > 100) {
                mx[x+y*w] = (abs(bx-x)>0 || abs(by-y)>0) ? -1 : 0;
            } else {
                mx[x+y*w] = 0;
            }
            */
            
            mx[x+y*w] = ((128+wx*7)<<16) | (128+wy*7);
        }
    }
}


int *difference(int *frame, int *bkg, int w, int h) 
{
    int *dif, *dif2, x, y, i, j;
    int r0, g0, b0, r1, g1, b1, c0, c1;
    
    dif = malloc(w*h*sizeof(int));
    dif2 = malloc(w*h*sizeof(int));

    i = 0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            r0 = frame[i] & 255;
            g0 = (frame[i]>>8) & 255;
            b0 = frame[i] >> 16;
            r1 = bkg[i] & 255;
            g1 = (bkg[i]>>8) & 255;
            b1 = bkg[i] >> 16;
            
            /* j = (abs(r0-r1) + abs(g0-g1) + abs(b0-b1)) / 3; */
            j = (abs(r0-r1)<<16) | (abs(g0-g1)<<8) | abs(b0-b1);
            dif[i] = j;

            i++;
        }
    }
    
    /*blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);*/

    i = 0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            c1 = dif[i];
            /* c1 = (c1 > 32) ? 255 : 0; */
            
            /* dif[i] = c1 * 0x010101; */
            
            i++;
        }
    }
    
    free(dif2);
    return dif;
}

char command[400];
int *bkg;

void blur_repeatedly(int *dif, int w, int h)
{
    int *dif2 = malloc(w*h*sizeof(int));
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
/*    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);
    blur(dif, dif2, w, h);
    blur(dif2, dif, w, h);*/
    free(dif2);
}


unsigned char *to_gray(int *img, int w, int h)
{
    int i, x, y;
    unsigned char *uc;
    
    uc = malloc(w*h);

    i = 0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            uc[i] = ((img[i]&255) + ((img[i]>>8)&255) + (img[i]>>16)) / 3;
            i++;
        }
    }
    
    return uc;
}

float VxMax = 0, VyMax = 0;

int *vec_to_color(float *Vx, float *Vy, int w, int h)
{
    int i, x, y, r, b;
    int *uc;
    
    uc = malloc(sizeof(int)*w*h);
    
    i = 0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            r = Vx[i] + 128;
            b = Vy[i] + 128;
            uc[i] = (r<<16) | b;
            i++;
        }
    }
    
    return uc;
}


void horn(unsigned char *f1, unsigned char *f2, unsigned char *f3, 
    float **Vx_p, float **Vy_p, int w, int h)
{
    int i, x, y, n, a, b;
    float Ix, Iy, It, Vxbar, Vybar, p, q;
    float *Vx, *Vy, *Vx2, *Vy2;
    
    Vx2 = malloc(sizeof(float)*w*h);
    Vy2 = malloc(sizeof(float)*w*h);
    
    Vx = *Vx_p;
    Vy = *Vy_p;
    
    i = 0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            if (x==0) {
                Ix = f2[i+1] - f2[i];
            } else if (x==w-1) {
                Ix = f2[i] - f2[i-1];
            } else {
                Ix = (f2[i+1] - f2[i-1]) / 2.0;
            }
            if (y==0) {
                Iy = f2[i+w] - f2[i];
            } else if (y==h-1) {
                Iy = f2[i] - f2[i-w];
            } else {
                Iy = (f2[i+w] - f2[i-w]) / 2.0;
            }
            
            It = (f3[i] - f2[i]);
            
            Vxbar = 0;
            Vybar = 0;
            n = 0;
            
            if (x>0) {
                Vxbar += Vx[i-1];
                Vybar += Vy[i-1];
                n++;
            }
            if (y>0) {
                Vxbar += Vx[i-w];
                Vybar += Vy[i-w];
                n++;
            }
            if (x<w-1) {
                Vxbar += Vx[i+1];
                Vybar += Vy[i+1];
                n++;
            }
            if (y<h-1) {
                Vxbar += Vx[i+w];
                Vybar += Vy[i+w];
                n++;
            }
            Vxbar += Vx[i];
            Vybar += Vy[i];
            n++;
            
            /*for (b=-(y!=0)*w; b<=(y!=h-1)*w; b++) {
                for (a=-(x!=0); a<=(x!=w-1); a++) {
                    Vxbar += Vx[i+a+b];
                    Vybar += Vy[i+a+b];
                    n++;
                }
            }*/
            Vxbar /= n;
            Vybar /= n;
            
            /* printf("Ix=%f Iy=%f It=%f Vxbar=%f Vybar=%f n=%d i=%d\n",
                Ix, Iy, It, Vxbar, Vybar, n, i); */
            
            p = (Ix*Vxbar + Iy*Vybar + It);
            q = (1 + Ix*Ix + Iy*Iy);
            p /= q;
            Vx2[i] = Vxbar - Ix*p;
            Vy2[i] = Vybar - Iy*p;
            
            i++;
        }
    }
    
    *Vx_p = Vx2;
    *Vy_p = Vy2;
    free(Vx);
    free(Vy);
}
            

long long max_d0 = 34;
int max_d1 = 0;

int convol[5][5] = {
    0, 1, 2, 1, 0,      /* 4 */
    1, 2, 3, 2, 1,      /* 9 */
    2, 3, 4, 3, 2,      /* 14 */
    1, 2, 3, 2, 1,      /* 9 */
    0, 1, 2, 1, 0};     /* 4 */


int *compare3(int **frames, int w, int h)
{
    int x, y, i, *dif, p1, p2, p3, d21, d32, d31;
    int r1, r2, r3;
    int g1, g2, g3;
    int b1, b2, b3;
    int j, k, mdc=0, *l;
    
    max_d1 = 0;
    dif = malloc(w*h*sizeof(int));
    
    i = w*(h-1);
    for (x=0; x<w; x++) {
        dif[x] = 0;
        dif[x+w] = 0;
        dif[i-w] = 0;
        dif[i++] = 0;
    }
    for (y=0; y<h; y++) {
        dif[y*w+1] = 0;
        dif[y*w] = 0;
        dif[y*w+w-1] = 0;
        dif[y*w+w-2] = 0;
    }
    
    i=w*2;
    for (y=2; y<h-2; y++) {
        i+=2;
        for (x=2; x<w-2; x++) {
            d21 = 0;
            d32 = 0;
            d31 = 0;
            
            l = convol;
            for (k=-w*2; k<=w*2; k+=w) {
                for (j=-2; j<=2; j++) {
                    p1 = frames[0][i+j+k];
                    p2 = frames[1][i+j+k];
                    p3 = frames[2][i+j+k];
                    
                    r1 = p1 >> 16;
                    g1 = (p1 >> 8) & 255;
                    b1 = p1 & 255;
        
                    r2 = p2 >> 16;
                    g2 = (p2 >> 8) & 255;
                    b2 = p2 & 255;
        
                    r3 = p3 >> 16;
                    g3 = (p3 >> 8) & 255;
                    b3 = p3 & 255;
                    
                    p1 = abs(r2 - r1) + abs(g2 - g1) + abs(b2 - b1);
                    /* if (p1 > d21) d21 = p1; */
                    p2 = abs(r3 - r1) + abs(g3 - g1) + abs(b3 - b1);
                    /* if (p2 > d31) d31 = p2; */
                    p3 = abs(r3 - r2) + abs(g3 - g2) + abs(b3 - b2);
                    /* if (p3 > d32) d32 = p3; */
                    
                    d21 += p1 * *l;
                    d31 += p2 * *l;
                    d32 += p3 * *l;
                    
                    l++;
                }
            }
            
            d21 /= 40;
            d31 /= 40;
            d32 /= 40;
            
            /* if (d21 > max_d1) max_d1 = d21;
            if (d32 > max_d1) max_d1 = d32;
            if (d31 > max_d1) max_d1 = d31;
            max_d1 += d21;
            max_d1 += d32; */
            max_d1 += d31;
            mdc += 1;
            
            j = 0;
            if (d21 > max_d0 && d32 > max_d0) j = 255;
            /* k = d21;
            if (d32 < k) k = d32;
            j = (d21/3) | ((d32/3)<<16) << ((k/3)<<8);
            j = d31;
            if (j>255) j = 255; */
            dif[i] = j;
            /* if (d21 > 4*(d31+2) || d32 > 4*(d31+2)) dif[i] = 255;
            
            if (y<=1 || y>=h-2 || x<=1 || x>=w-2) dif[i] = 0;
           
            j = 0;
            if (d21 > d31*10 && d32 > d31*10) j = -1;
            
            j = (d21-d31);
            if (j<0) j = 0;
            k = (d32-d31);
            if (k<0) k = 0;
            
            j = (j+k);
            if (j>max_d) max_d = j;
            
            if (max_d) {
                j = j * 255 / max_d;
            } else {
                j = 0;
            }
            
            j = (d21/3) | ((d32/3)<<16) | ((d31/3)<<8);
          
            dif[i] = j * 0x010101;
            */
            
            i++;
        }
        i+=2;
    }
    
    j = max_d1 / mdc;
    if (j > max_d0) max_d0 = j;
    /* max_d0 = max_d1 / mdc; */
    /* printf("%d\n", max_d0); */
    
    return dif;
}


int *mode(int **frames, int w, int h)
{
    int i, x, y, f, k, p, l, t, a, b;
    int *m;
    int colors[32];
    int counts[32];
    
    m = malloc(sizeof(int)*w*h);
    
    i=0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            memset(counts, 0, sizeof(counts));
            for (f=0; f<16; f++) {
                p = frames[f][i];
                for (k=0; k<16; k++) {
                    if ((p & 0xfcfcfc) == (colors[k] & 0xfcfcfc)) {
                        counts[k]++;
                        while (k>0 && counts[k] > counts[k-1]) {
                            t = counts[k];
                            counts[k] = counts[k-1];
                            counts[k-1] = t;
                            t = colors[k];
                            colors[k] = colors[k-1];
                            colors[k-1] = t;
                            k--;
                        }
                        break;
                    }
                    if (!counts[k]) {
                        counts[k] = 1;
                        colors[k] = p;
                        break;
                    }
                }
            }
            m[i] = colors[0];
            i++;
        }
    }
    
    return m;
}


int *cut_singles(int *dif, int w, int h, int level)
{
    int i, x, y, a, b, f, change=0;
    int *dif2;
    
    dif2 = malloc(sizeof(int)*w*h);
    memcpy(dif2, dif, sizeof(int)*w*h);
    
    i=w*2;
    for (y=2; y<h-2; y++) {
        i+=2;
        for (x=2; x<w-2; x++) {
            if (!dif[i]) {
                i++;
                continue;
            }
            
            f = 0;
            for (b=-w*2; b<=w*2; b+=w) {
                for (a=-2; a<=2; a++) {
                    if (!a && !b) continue;
                    if (dif[i+a+b]) {
                        f++;
                        /* goto next; */
                    }
                }
            }
            next:
            if (f<level) {
                dif2[i] = 0;
                change = 1;
            }
            
            i++;
        }
        i += 2;
    }
    
    if (change) {
        free(dif);
        return dif2;
    } else {
        free(dif2);
        return dif;
    }
}


int *add_missing(int *dif, int w, int h)
{
    int i, x, y, a, b, c, d, changed=0;
    int *dif2;
    double f;
    
    dif2 = malloc(sizeof(int)*w*h);
    memcpy(dif2, dif, sizeof(int)*w*h);
    
    i=w*4;
    for (y=4; y<h-4; y++) {
        i+=4;
        for (x=4; x<w-4; x++) {
            if (dif[i]) {
                i++;
                continue;
            }
            
            f = 0;
            
            /* Top/bottom, near */
            b = -w;
            d = w;
            for (a=-1, c=1; a<=1; a++, c--) {
                if (dif[i+a+b] && dif[i+c+d]) {
                    /* f = 1; */
                    /* goto next; */
                    f+=1;
                }
            }
            
            if (f>=2) goto next;

            /* left/right, near */
            if (dif[i-1] && dif[i+1]) {
                f+=1;
                /* f = 1; */
                /* goto next; */
            }

            if (f>=2) goto next;
            
            /* top/bottom, far */
            b = -w*2;
            d = w*2;
            for (a=-2, c=2; a<=2; a++, c--) {
                if (dif[i+a+b] && dif[i+c+d]) {
                    /* f = 1; */
                    /* goto next; */
                    f+=0.5;
                }
            }

            if (f>=2) goto next;
            
            /* left/right, far */
            a = -2;
            c = 2;
            for (b=-w*1, d=w*1; b<=w*1; b+=w, d-=w) {
                if (dif[i+a+b] && dif[i+c+d]) {
                    /* f = 1; */
                    /* goto next; */
                    f+=0.5;
                }
            }

            if (f>=2) goto next;

#if 1
            /* top/bottom, farther */
            b = -w*3;
            d = w*3;
            for (a=-3, c=3; a<=3; a++, c--) {
                if (dif[i+a+b] && dif[i+c+d]) {
                    /* f = 1; */
                    /* goto next; */
                    f+=0.33;
                }
            }

            if (f>=2) goto next;
            
            /* left/right, farther */
            a = -3;
            c = 3;
            for (b=-w*2, d=w*2; b<=w*2; b+=w, d-=w) {
                if (dif[i+a+b] && dif[i+c+d]) {
                    /* f = 1; */
                    /* goto next; */
                    f+=0.33;
                }
            }
#endif

            if (f>=2) goto next;

#if 1
            /* top/bottom, farther 2 */
            b = -w*4;
            d = w*4;
            for (a=-4, c=4; a<=4; a++, c--) {
                if (dif[i+a+b] && dif[i+c+d]) {
                    /* f = 1; */
                    /* goto next; */
                    f+=0.2;
                }
            }

            if (f>=2) goto next;
            
            /* left/right, farther 2 */
            a = -4;
            c = 4;
            for (b=-w*3, d=w*3; b<=w*3; b+=w, d-=w) {
                if (dif[i+a+b] && dif[i+c+d]) {
                    /* f = 1; */
                    /* goto next; */
                    f+=0.2;
                }
            }
#endif

            next:
            if (f>=2) {
                dif2[i] = 0xff0000;
                changed = 1;
            }
            
            /*f = 0;
            for (b=-w*2; b<=w*2; b+=w) {
                for (a=-2; a<=2; a++) {
                    if (!a && !b) continue;
                    if (dif[i+a+b]) {
                        f++;
                        goto next;
                    }
                }
            }
            next:
            if (f>4) dif2[i] = 0xff0000;*/
            
            i++;
        }
        i += 4;
    }
    
    if (changed) {
        free(dif);
        return dif2;
    } else {
        free(dif2);
        return dif;
    }
}

unsigned short equiv[65536];


int *label(int *img, int w, int h)
{
    int *lb, lbn=0, i, x, y, t, j, k;
    int nlb[3];
    
    memset(equiv, 0, sizeof(equiv));
    
    lb = malloc(sizeof(int)*w*h);
    memset(lb, 0, sizeof(int)*w*h);
    
    i=0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            if (!img[i]) {
                i++;
                continue;
            }
            
            nlb[0] = 0;
            nlb[1] = 0;
            nlb[2] = 0;
            
            if (x>0 && img[i-1]) {
                nlb[0] = lb[i-1];
            }
            if (y>0 && img[i-w]) {
                nlb[1] = lb[i-w];
            }
            if (x>0 && y>0 && img[i-w-1]) {
                nlb[2] = lb[i-w-1];
            }
            
            /* Trace back to the furthest ancestor */
            for (j=0; j<3; j++) {
                while (nlb[j] && equiv[nlb[j]]) nlb[j] = equiv[nlb[j]];
            }
            
            /* Sort in ascending order */
            k = 1;
            while (k) {
                k = 0;
                for (j=0; j<2; j++) {
                    if (nlb[j] < nlb[j+1]) {
                        t = nlb[j];
                        nlb[j] = nlb[j+1];
                        nlb[j+1] = t;
                        k = 1;
                    }
                }
            }
            
            if (nlb[0] && nlb[1] && nlb[0] != nlb[1]) {
                equiv[nlb[1]] = nlb[0];
            }
            if (nlb[0] && nlb[2] && nlb[0] != nlb[2]) {
                equiv[nlb[2]] = nlb[0];
            }
            if (!nlb[0] && nlb[1] && nlb[2] && nlb[1] != nlb[2]) {
                equiv[nlb[2]] = nlb[1];
            }
            
            if (nlb[0]) {
                lb[i] = nlb[0];
            } else
            if (nlb[1]) {
                lb[i] = nlb[1];
            } else
            if (nlb[2]) {
                lb[i] = nlb[2];
            } else {
                lb[i] = ++lbn;
            }
            
            i++;
        }
    }

    i=0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            if (!lb[i]) {
                i++;
                continue;
            }
            
            t = lb[i];
            while (equiv[t] && equiv[t] != t) t = equiv[t];
            lb[i] = t;
            
            i++;
        }
    }

/*    i=0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            if (!lb[i]) {
                i++;
                continue;
            }
            
            t = lb[i];
     
            lb[i] = ((t&3) << 6) | ((t&0xc) << 12) | ((t&0x30) << 18);
            
            i++;
        }
    }*/

    return lb;
}


typedef struct {
    long long center_x, center_y;
    int min_x, min_y, max_x, max_y;
    int area;
} labeled_t;

labeled_t labeled[65536];

void output_labeled(int *lb, int w, int h)
{
    int x, y, i, j, more;
    
    memset(labeled, 0, sizeof(labeled));
    
    for (i=0; i<65536; i++) {
        labeled[i].min_x = 1000000;
        labeled[i].min_y = 1000000;
    }
    
    i=0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            if (!lb[i]) {
                i++;
                continue;
            }
            
            labeled[lb[i]].center_x += x;
            labeled[lb[i]].center_y += y;
            if (x < labeled[lb[i]].min_x) labeled[lb[i]].min_x = x;
            if (x > labeled[lb[i]].max_x) labeled[lb[i]].max_x = x;
            if (y < labeled[lb[i]].min_y) labeled[lb[i]].min_y = y;
            if (y > labeled[lb[i]].max_y) labeled[lb[i]].max_y = y;
            labeled[lb[i]].area++;
            
            i++;
        }
    }

    for (i=0; i<65536; i++) {
        if (!labeled[i].area) continue;
        
        sprintf(msg, "<Acquisition id=\"%d\" x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\"/>",
            i, 
            (int)(labeled[i].center_x / labeled[i].area), 
            (int)(labeled[i].center_y / labeled[i].area),
            labeled[i].max_x - labeled[i].min_x + 1,
            labeled[i].max_y - labeled[i].min_y + 1);
        /*
        fprintf(stderr, "decode: SENDING %s\n", msg);
        write(clifd, msg, strlen(msg));
        get_ok();
        */
        printf("%s\n", msg);
    }
}


int *blend(int *ia, int *ib, int w, int h)
{
    int *ic = malloc(sizeof(int)*w*h);
    int x, y, i, r, g, b;
    
    i=0;
    for (y=0; y<h; y++) {
        for (x=0; x<w; x++) {
            b = (ia[i] & 255) + (ib[i] & 255);
            g = ((ia[i] >> 8) & 255) + ((ib[i] >> 8) & 255);
            r = ((ia[i] >> 16) & 255) + ((ib[i] >> 16) & 255);
            ic[i] = ((r/2)<<16) | ((g/2)<<8) | (b/2);
            i++;
        }
    }
    
    return ic;
}

int *framesa[65], *framesb[3];

int main(int argc, char *argv[])
{
    FILE *in, *out;
    int *buf, *dif, *dif2, *lb, *bl, *b1, *b2, *b3, *b4, *b5, *b6, *b7;
    int w, h, i;
    unsigned char *g1, *g2, *g3;
    XImage *img;
    XEvent event;
    char framemsg[15];
    
    if (argc<2) {
        printf("Need filename (in PPM format)\n");
        exit(0);
    }

    /*socket_init("layer-1");*/
    
    xinit();

    printf("<Frames>\n");

    /* Get START */
    /*
    char start[6];
    fprintf(stderr, "decode: WAITING FOR START\n");
    while((0 ==  read(clifd, start, 6)) || (0 != strncmp(start, "START", 5)));
    fprintf(stderr, "decode: GOT START\n");
    */

    /* in = fopen("background_e.ppm", "rb"); */
    /* bkg = read_image(in, &w, &h); */
    /* fclose(in); */


    img = XCreateImage(display, visual, depth, ZPixmap, 0, 0, 640, 480, 32, 0);
    
    
    /*buf = read_image(in, &w, &h);
    free(buf);
    buf = read_image(in, &w, &h);
    free(buf);*/
            
    /*buf = read_image(in, &w, &h);
    free(buf);
    buf = read_image(in, &w, &h);
    frames[1] = buf;
    
    buf = read_image(in, &w, &h);
    free(buf);
    buf = read_image(in, &w, &h);
    frames[2] = buf;

    frames[0] = 0;*/

    /* Read 64 frames */
    in = fopen(argv[1], "r");
    for (i=1; i<33; i++) {
        buf = read_image(in, &w, &h);
        free(buf);
        buf = read_image(in, &w, &h);
        buf = color_blur(buf, w, h);
        buf = color_blur(buf, w, h);
        /*
        buf = color_blur(buf, w, h);
        buf = color_blur(buf, w, h);
        */
        framesa[i] = buf;
    }
    framesa[0] = 0;

    framenum = 16; 
    while (!feof(in)) {
        buf = read_image(in, &w, &h);
        if (!buf) break;
        free(buf);
        buf = read_image(in, &w, &h);
        if (!buf) break;
        framenum++; 
        
        buf = color_blur(buf, w, h);
        buf = color_blur(buf, w, h);
        /* buf = color_blur(buf, w, h); */
        /* buf = color_blur(buf, w, h); */
        
        if (framesa[0]) free(framesa[0]);
        for (i=0; i<32; i++) {
            framesa[i] = framesa[i+1];
        }
        framesa[32] = buf;
        
        
        
        /*b1 = blend(framesa[0], framesa[1], w, h);
        b2 = blend(framesa[2], framesa[3], w, h);
        b3 = blend(framesa[4], framesa[5], w, h);
        b4 = blend(framesa[6], framesa[7], w, h);
        b5 = blend(b1, b2, w, h);
        b6 = blend(b3, b4, w, h);
        b7 = blend(b5, b6, w, h);
        framesb[0] = b7; */ /* framesa[7]; */
        b1 = mode(framesa, w, h);
        framesb[0] = b1;
        /* free(b1); free(b2); free(b3); free(b4); free(b5); free(b6); */
        
        framesb[1] = framesa[16];

        /*b1 = blend(framesa[9], framesa[10], w, h);
        b2 = blend(framesa[11], framesa[12], w, h);
        b3 = blend(framesa[13], framesa[14], w, h);
        b4 = blend(framesa[15], framesa[16], w, h);
        b5 = blend(b1, b2, w, h);
        b6 = blend(b3, b4, w, h);
        b7 = blend(b5, b6, w, h);
        framesb[2] = b7; */ /* framesa[9]; */
        /* free(b1); free(b2); free(b3); free(b4); free(b5); free(b6);*/
        b1 = mode(framesa+17, w, h);
        framesb[2] = b1;
        
        /* printf("%d %d\n", w, h); */
        
        /*img->data = (char *)framesb[0];
        XPutImage(display, window, gc, img, 0, 0, 0, 480, 640, 480);
        XFlush(display);
        img->data = (char *)framesb[2];
        XPutImage(display, window, gc, img, 0, 0, 640, 480, 640, 480);
        XFlush(display);*/

        img->data = (char *)framesb[1];
        XPutImage(display, window, gc, img, 0, 0, 0, 0, 640, 480);
        
        /* dif = difference(buf, bkg, w, h); */
        dif = compare3(framesb, w, h);
        lb = label(dif, w, h);
        bl = blend(dif, framesb[1], w, h);

        img->data = bl /*(char *)framesb[0]*/;
        XPutImage(display, window, gc, img, 0, 0, 640, 0, 640, 480);

        /* Frame count */
        sprintf(framemsg, "Frame %d, time %0.2fs", framenum, ((double)framenum) / 3.0);
        XSetForeground(display, gc, 0xf8b395);
        XDrawString(display, window, gc, 10, 470, framemsg, strlen(framemsg));
        XDrawString(display, window, gc, 650, 470, framemsg, strlen(framemsg));
        XFlush(display);

        /*fprintf(stderr, "decode: SENDING <Frame>\n");*/
        /*write(clifd, "<Frame>", 8);*/
        printf("<Frame time=\"%f\" number=\"%d\">\n", ((double)framenum) / 3.0, framenum);
        output_labeled(lb, w, h);
        /*fprintf(stderr, "decode: SENDING </Frame>\n");*/
        /*write(clifd, "</Frame>", 9);*/
        printf("</Frame>");

        free(dif);
        free(bl);
        free(lb);

        /*for (;;) {
            dif2 = cut_singles(dif, w, h, 3);
            if (dif2 == dif) break;
            dif = dif2;
        }
        
        for (;;) {
            dif2 = add_missing(dif, w, h);
            if (dif2 == dif) break;
            dif = dif2;
        }

        for (;;) {
            dif2 = cut_singles(dif, w, h, 8);
            if (dif2 == dif) break;
            dif = dif2;
        }*/

        
        /*g1 = to_gray(framesb[0], w, h);
        g2 = to_gray(framesb[1], w, h);
        g3 = to_gray(framesb[2], w, h);
        b1 = malloc(sizeof(int)*w*h);
        b2 = malloc(sizeof(int)*w*h);
        memset(b1, 0, sizeof(int)*w*h);
        memset(b2, 0, sizeof(int)*w*h);*/
        
        /*for (i=0; i<100; i++) {
            horn(g1, g2, g3, &b1, &b2, w, h);
            b3 = vec_to_color(b1, b2, w, h);
            
            blur_repeatedly(dif, w, h);
            img->data = (char *)b3;
            XPutImage(display, window, gc, img, 0, 0, 640, 0, 640, 480);
            XFlush(display);
            
            free(lb);
            free(dif);
            free(bl);
            free(b3);
        }*/
    
        /* do {
                XNextEvent(display, &event);
        } while (event.type != KeyPress);        
        
        free(g1); free(g2); free(g3); free(b1); free(b2);

        free(buf);
        */
        free(framesb[0]);
        free(framesb[2]);

        /*get_next();*/
    }

    printf("</Frames>\n");

    /*do {
            XNextEvent(display, &event);
    } while (event.type != KeyPress);        */
    
    img->data = 0;
    XDestroyImage(img);
    xbye();

    /*socket_close();*/
    
    return 0;
}

