// gcc `pkg-config --libs glew freeglut` -lm astrattismo.c

#include <GL/glut.h>
#include <stdlib.h>
#include <math.h>

# if 0
    % An extract from octave history
    a = imread ("2018-04-08-115223_1920x1080_scrot.png");
    r = a(:,:, 1);
    imshow (r)
    size(r) / 2
    imshow (r(510:570, 930:990))
    imshow (r(500-10:559+10, 940-10:999+10))
    rcl = r(500-10:559+10, 940-10:999+10))
    rcl = r(500-10:559+10, 940-10:999+10);
    size(rcl)
    imshow (rcl)
    rcl(1, :)
    [rcl(1, :)'; rcl(:, end); rcl(end, :)'; rcl(:, 1)]
    plot([rcl(1, :)'; rcl(:, end); rcl(end, :)'; rcl(:, 1)])
    plot([rcl(1, :)'; rcl(:, end); rcl(end, :)'; rcl(:, 1)] > 123)
    pea = [rcl(1, :)'; rcl(:, end); rcl(end, :)'; rcl(:, 1)]
    plot(pea)
    plot(conv(pea,  [-1 1]))
    plot(conv(pea > 123,  [-1 1]))
    plot(conv(pea > 123,  [-1 1]) == 1)
    find(conv(pea > 123,  [-1 1]) == 1)
    size(find(conv(pea > 123,  [-1 1]) == 1))
    size(r) / 2
    imshow ( r(540:540+12, 960:1920) )
    imshow ( r(540:end, 960:960+12) )
    imshow ( r(540:end, 960+11:960+12) )
    plot(r(540:end, 960+11:960+12))
    imshow ( r(540:end, 960:960+12) )
    kkk = r(540:end, 960:960+12)
    plot(r(540+55:end, 960+11:960+12))
    plot(r(540+55:end, 960+11:960+11))
    s= (r(540+55:end, 960+11:960+11))
    plot(s)
    plot(conv(s, [1,1,1]))
    plot(conv(s, [-1, 1, -1]))
    plot(conv(s, [-1, 1]))
    plot(conv(s, [-1, 1]) > 10)
    ss  = (conv(s, [-1, 1]) > 10)
    conv(ss, [-1, 1])
    plot(conv(ss, [-1, 1]))
    fond(conv(ss, [-1, 1]) == 1)
    find(conv(ss, [-1, 1]) == 1)
    r = find(conv(ss, [-1, 1]) == 1)
    plot(r)
    plot(r / 488)
    r / 488
    polyfit(linspace(0, 1, size(r, 1))', r, 2)
    polyfit(linspace(0, 1, size(r, 1))', r/ 488, 2)
    r+55
    polyfit(linspace(0, 1, size(r, 1))', r/ 543, 2)
    x = (linspace(0, 1, 20))
    plot(x, x^2 * 0.8418434 + -0.0054162 * x + 0.0338263)
    plot(x, x.^2 * 0.8418434 + -0.0054162 * x + 0.0338263)
#endif

void
display(void) {
    int tn = 72;
    int rn = 24;
    float dt = 2.0 / tn;
    float dl = 1.0 / rn;
    float p2 = +0.8418434;
    float p1 = -0.0054162;
    float p0 = +0.0338263 + 0.1;
    float t, l;
    int i, j;
    
    glClearColor(1., 1., 1., 0.);
    glClear(GL_COLOR_BUFFER_BIT);
    glBegin(GL_TRIANGLES);
    glColor3f(0, 0, 0.65);

    for (l = 1.0, j = 0; j < rn; j++, l -= dl) {
        for (t = -1.0, i = 0; i < tn; i++, t += dt) {
            float r = l * l * p2 + l * p1 + p0;
            float nr = (l - dl) * (l - dl) * p2 + (l - dl) * p1 + p0;
            float dr = nr - r;
            
            float x0  = (r + 0.) * cos((t + 0.  ) * M_PI);
            float y0  = (r + 0.) * sin((t + 0.  ) * M_PI);
            float x1  = (r + dr) * cos((t + 0.  ) * M_PI);
            float y1  = (r + dr) * sin((t + 0.  ) * M_PI);
            float x2  = (r + dr) * cos((t + dt  ) * M_PI);
            float y2  = (r + dr) * sin((t + dt  ) * M_PI);
            float x3  = (r + 0.) * cos((t + dt  ) * M_PI);
            float y3  = (r + 0.) * sin((t + dt  ) * M_PI);
            float x2h = (r + dr) * cos((t + dt/2) * M_PI);
            float y2h = (r + dr) * sin((t + dt/2) * M_PI);
            float x3h = (r + 0.) * cos((t + dt/2) * M_PI);
            float y3h = (r + 0.) * sin((t + dt/2) * M_PI);

            if (i == tn * 1/4 || i == tn * 3/4) {
                glVertex2f(x1, y1);
                glVertex2f(x2, y2);
                glVertex2f(x3h, y3h);
            } else if (i == 0 || i == tn * 1/2) {
                glVertex2f(x0, y0);
                glVertex2f(x1, y1);
                glVertex2f(x2h, y2h);
                glVertex2f(x2h, y2h);
                glVertex2f(x2, y2);
                glVertex2f(x3, y3);
            } else if (t + 1. < 2.0 * 1/4) {
                glVertex2f(x1, y1);
                glVertex2f(x2, y2);
                glVertex2f(x3, y3);
            } else if (t + 1. < 2.0 * 2/4) {
                glVertex2f(x0, y0);
                glVertex2f(x1, y1);
                glVertex2f(x2, y2);
            } else if (t + 1. < 2.0 * 3/4) {
                glVertex2f(x1, y1);
                glVertex2f(x2, y2);
                glVertex2f(x3, y3);
            } else if (t + 1. < 2.0 * 4/4) {
                glVertex2f(x0, y0);
                glVertex2f(x1, y1);
                glVertex2f(x2, y2);
            }
        }
    }
    for (l = 1.0, t = -1.0, i = 0; i < tn; i++, t += dt) {
        float r = l * l * p2 + l * p1 + p0;
        float x0 = (r + 0.) * cos((t + 0.) * M_PI);
        float y0 = (r + 0.) * sin((t + 0.) * M_PI);
        float x1 = (r + .5) * cos((t + 0.) * M_PI);
        float y1 = (r + .5) * sin((t + 0.) * M_PI);
        float x2 = (r + .5) * cos((t + dt) * M_PI);
        float y2 = (r + .5) * sin((t + dt) * M_PI);
        float x3 = (r + 0.) * cos((t + dt) * M_PI);
        float y3 = (r + 0.) * sin((t + dt) * M_PI);
        if (i % 2) {
            continue;
        }
        glVertex2f(x0, y0);
        glVertex2f(x1, y1);
        glVertex2f(x2, y2);
        glVertex2f(x0, y0);
        glVertex2f(x3, y3);
        glVertex2f(x2, y2);
    }
    glEnd();
    glFlush();
}

void
keyboard(unsigned char k, int x, int y) {
    if (k == 'q' || k == 27) {
        exit(1);
    }
}

int
main(int argc, char *argv[])
{
    glutInit(&argc, argv);
    glutInitWindowSize(640,500);
    glutInitWindowPosition(1,1);
    glutCreateWindow("ASTRATTISMO - franco delfino");
    glutDisplayFunc(display);
    glutKeyboardFunc(keyboard);
    glutMainLoop();

    return 0;
}
