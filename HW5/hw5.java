/* Name: Aland Kuang

   UID: 104157973

   Others With Whom I Discussed Things:

   Other Resources I Consulted:

*/

import java.io.*;
import java.util.Arrays;
import java.lang.*;
import java.util.concurrent.RecursiveTask;
import java.util.stream.IntStream;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
        R = r;
        G = g;
        B = b;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}


// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    public PPMImage(int w, int h, int m, RGB[] p) {
        width = w;
        height = h;
        maxColorVal = m;
        pixels = p;
    }

    // parse a PPM image file named fname and produce a new PPMImage object
    public PPMImage(String fname)
            throws FileNotFoundException, IOException {
        FileInputStream is = new FileInputStream(fname);
        BufferedReader br = new BufferedReader(new InputStreamReader(is));
        br.readLine(); // read the P6
        String[] dims = br.readLine().split(" "); // read width and height
        int width = Integer.parseInt(dims[0]);
        int height = Integer.parseInt(dims[1]);
        int max = Integer.parseInt(br.readLine()); // read max color value
        br.close();

        is = new FileInputStream(fname);
        // skip the first three lines
        int newlines = 0;
        while (newlines < 3) {
            int b = is.read();
            if (b == 10)
                newlines++;
        }

        int MASK = 0xff;
        int numpixels = width * height;
        byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
        RGB[] pixels = new RGB[numpixels];
        for (int i = 0; i < numpixels; i++) {
            int offset = i * 3;
            pixels[i] = new RGB(bytes[offset] & MASK,
                    bytes[offset+1] & MASK,
                    bytes[offset+2] & MASK);
        }
        is.close();

        this.width = width;
        this.height = height;
        this.maxColorVal = max;
        this.pixels = pixels;
    }

    // write a PPMImage object to a file named fname
    public void toFile(String fname) throws IOException {
        FileOutputStream os = new FileOutputStream(fname);

        String header = "P6\n" + width + " " + height + "\n"
                + maxColorVal + "\n";
        os.write(header.getBytes());

        int numpixels = width * height;
        byte[] bytes = new byte[numpixels * 3];
        int i = 0;
        for (RGB rgb : pixels) {
            bytes[i] = (byte) rgb.R;
            bytes[i+1] = (byte) rgb.G;
            bytes[i+2] = (byte) rgb.B;
            i += 3;
        }
        os.write(bytes);
        os.close();
    }

    public static void main(String[] args){
        String filename = args[0];
        try {
            PPMImage newimage = new PPMImage("florence.ppm");
            PPMImage inc = new PPMImage("inc.ppm");

            PPMImage negated = newimage.negate();

            negated.toFile("negated.ppm");

            PPMImage greyscale = newimage.greyscale();

            greyscale.toFile("greyscale.ppm");

            PPMImage mirrored = newimage.mirrorImage();
            PPMImage incmirror = inc.mirrorImage();
            PPMImage incmirrorstream = inc.mirrorImage2();

            mirrored.toFile("mirrored.ppm");
            incmirror.toFile("incmirror.ppm");
            incmirrorstream.toFile("incmirrorstream.ppm");

            long start = System.nanoTime();
            PPMImage gaussed = newimage.gaussianBlur(20, 2.0);
            long end = System.nanoTime();
            //System.out.println("Execution time:" + (end - start)/ 1000000000.0);
            gaussed.toFile("gaussed.ppm");



        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private boolean checkfornulls(){
        boolean flag = false;
        for(int i = 0; i < pixels.length; i++){
            if(pixels[i] == null){
                System.out.println(i);
                return true;
            }
        }
        return flag;
    }

    // implement using Java 8 Streams
    public PPMImage negate() {
        RGB[] negimg = Arrays.stream(pixels)
                        .parallel()
                        .map(pix -> new RGB(maxColorVal - pix.R, maxColorVal - pix.G, maxColorVal - pix.B))
                        .toArray(size -> new RGB[size]);

        return new PPMImage(width, height, maxColorVal, negimg);
    }

    // implement using Java 8 Streams
    public PPMImage greyscale() {
        RGB[] greyimg = Arrays.stream(pixels)
                        .parallel()
                    .map(pix -> new RGB((int) Math.round(0.299*pix.R + 0.587*pix.G + 0.114*pix.B),
                            (int) Math.round(0.299*pix.R + 0.587*pix.G + 0.114*pix.B),
                            (int) Math.round(0.299*pix.R + 0.587*pix.G + 0.114*pix.B)))
                    .toArray(size -> new RGB[size]);

        return new PPMImage(width, height, maxColorVal, greyimg);
    }

    // implement using Java's Fork/Join library
    public PPMImage mirrorImage() {
        RGB[] mirroredpixes = new MirrorTask(width, height, pixels, 0, height).compute();

        return new PPMImage(width, height, maxColorVal, mirroredpixes);
    }

    // implement using Java 8 Streams
    public PPMImage mirrorImage2() {
        //first create an empty array
        int numpixels = width * height;
        RGB[] mirrorpixes = new RGB[numpixels];
        IntStream.range(0, numpixels).parallel().forEach(oldimg_i -> {
                                                int modi = oldimg_i % width;
                                                mirrorpixes[(oldimg_i - modi) + (width - modi - 1)] = pixels[oldimg_i];
        });

        return new PPMImage(width, height, maxColorVal, mirrorpixes);
    }

    // implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {
        RGB[] gaussedpixes = new GaussianTask(width, height, 0, height, radius, pixels, Gaussian.gaussianFilter(radius, sigma) ).compute();

        return new PPMImage(width, height, maxColorVal, gaussedpixes);
    }



}

class MirrorTask extends RecursiveTask<RGB[]>{

    private RGB[] rgbarray;
    private int width, height, row_low, row_high;

    private final int SEQUENTIAL_CUTOFF =  100; //a really small image


    public MirrorTask(int w, int h, RGB[] rgb, int r_l, int r_h){
        width = w;
        height = h;
        rgbarray = rgb;
        row_low = r_l;
        row_high = r_h;

    }

    public RGB[] concatRGBs(RGB[] left, RGB[] right){
        int leftLen = left.length;
        int rightLen = right.length;
        RGB[] newRGBarray = new RGB[leftLen + rightLen];
        System.arraycopy(left, 0, newRGBarray, 0, leftLen);
        System.arraycopy(right, 0, newRGBarray, leftLen, rightLen);
        return newRGBarray;
    }


    @Override
    protected RGB[] compute() {


        if(row_high - row_low > SEQUENTIAL_CUTOFF){
            int mid = (row_high + row_low) / 2;

            MirrorTask left = new MirrorTask(width, height, rgbarray, row_low, mid);
            MirrorTask right = new MirrorTask(width, height, rgbarray, mid, row_high);
            left.fork();
            RGB[] rightpixes = right.compute();
            RGB[] leftpixes = left.join();

            return concatRGBs(leftpixes, rightpixes);

        }
        else {
            int mirrored_pix = 0;
            int mirrored_max = (row_high - row_low) * width;

            RGB[] mirrored_array = new RGB[mirrored_max];

            for(int curr_row = row_low; curr_row < row_high; curr_row ++){
                for(int counter = 0;  mirrored_pix < mirrored_max && counter < width; mirrored_pix++, counter++){

                    /*if(rgbarray[((curr_row + 1)*width) - counter - 1] == null){
                        System.out.println("yo");
                    }*/
                    mirrored_array[mirrored_pix] =  rgbarray[((curr_row + 1)*width) - counter - 1];
                }
            }//end for
            return mirrored_array;
        }
    }
}

class GaussianTask extends RecursiveTask<RGB[]>{

    private RGB[] rgbarray;
    private int width, height, row_low, row_high, radius;
    private double[][] gaussianfilter;

    private final int SEQUENTIAL_CUTOFF = 200; //a small image

    public GaussianTask(int w, int h, int r_l, int r_h, int gf_rad, RGB[] rgb, double[][] gf){
        width = w;
        height = h;
        row_low = r_l;
        row_high = r_h;
        radius = gf_rad;
        rgbarray = rgb;
        gaussianfilter = gf;
    }

    public RGB[] concatRGBs(RGB[] left, RGB[] right){
        int leftLen = left.length;
        int rightLen = right.length;
        RGB[] newRGBarray = new RGB[leftLen + rightLen];
        System.arraycopy(left, 0, newRGBarray, 0, leftLen);
        System.arraycopy(right, 0, newRGBarray, leftLen, rightLen);
        return newRGBarray;
    }

    private RGB getpixel(int row, int column){
        if(row < 0)
            row = 0;
        if(row >= height)
            row = height - 1;
        if (column < 0)
            column = 0;
        if(column >= width)
            column = width - 1;
        return rgbarray[row*width+column];
    }

    private RGB[][] pixelswithinradius(int start_row, int start_column){
        //to get all the pixels in the radius, we must compute a diameter
        int xysize = radius*2 + 1;
        RGB[][] manypixels = new RGB[xysize][xysize];

        //+1 for the central pixel!
        for(int i = 0; i < radius*2 + 1; i++){
            int tempcolumn = start_column;
            for(int j = 0; j < radius*2 + 1; j++){
                manypixels[i][j] = getpixel(start_row, tempcolumn);
                tempcolumn++;
            }//end for
            start_row++;
        }//end for

        return manypixels;
    }

    private RGB computeGaussianPixel(int row, int column){
        //we multiply by 2 in order to get to the upper left hand corner of the region we're trying to compute a gaussian
        int starting_row = row - radius;
        int starting_column = column - radius;

        RGB[][] pixelstocompute = pixelswithinradius(starting_row, starting_column);

        double newR = 0, newB = 0, newG = 0;
        for(int i = 0; i < radius*2+1; i++){
            for(int j = 0; j < radius*2 + 1; j++){
                newR += gaussianfilter[i][j]*((double) (pixelstocompute[i][j]).R);
                newB += gaussianfilter[i][j]*((double) (pixelstocompute[i][j]).B);
                newG += gaussianfilter[i][j]*((double) (pixelstocompute[i][j]).G);
            }//end for
        }//end for

        int finalR = (int) Math.round(newR);
        int finalB = (int) Math.round(newB);
        int finalG = (int) Math.round(newG);

        return new RGB(finalR, finalG, finalB);


    }

    @Override
    protected RGB[] compute() {

        if(row_high - row_low > SEQUENTIAL_CUTOFF){
            int mid = (row_high + row_low) / 2;
            GaussianTask left = new GaussianTask(width, height, row_low, mid, radius, rgbarray, gaussianfilter);
            GaussianTask right = new GaussianTask(width, height, mid, row_high, radius, rgbarray, gaussianfilter);
            left.fork();
            RGB[] rightgaussified = right.compute();
            RGB[] leftguassified = left.join();

            return concatRGBs(leftguassified, rightgaussified);
        }
        else {
            int curr_pix = 0;
            int max_pix = (row_high - row_low) * width;

            RGB[] gaussianarray = new RGB[max_pix];

            for(int curr_row = row_low; curr_row < row_high; curr_row++){
                for(int counter = 0; counter < width && curr_pix < max_pix; counter++, curr_pix++ ){
                    RGB gaussifiedpix = computeGaussianPixel(curr_row, counter);
                    gaussianarray[curr_pix] = gaussifiedpix;
                }//end for
            }//end for

            return gaussianarray;
        }

    }
}

class PicComparator{

    private PPMImage m_img1;
    private PPMImage m_img2;

    public PicComparator(PPMImage img1, PPMImage img2){
        m_img1 = img1;
        m_img2 = img2;
    }

    public boolean compare(){
        for(int i = 0; i < m_img1.pixels.length; i++){
            if(m_img1.pixels.length != m_img2.pixels.length){
                System.out.println("Not right lengths...");
                return false;
            }
            if(m_img1.pixels[i].B != m_img2.pixels[i].B || m_img1.pixels[i].G != m_img2.pixels[i].G || m_img1.pixels[i].R != m_img2.pixels[i].R){
                System.out.println("Difference occured at: " + i);
                return false;
            }
        }
        return true;
    }


}

// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
        return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
        int length = 2 * radius + 1;
        double[] hkernel = new double[length];
        for(int i=0; i < length; i++)
            hkernel[i] = gaussian(i, radius, sigma);
        double[][] kernel2d = new double[length][length];
        double kernelsum = 0.0;
        for(int i=0; i < length; i++) {
            for(int j=0; j < length; j++) {
                double elem = hkernel[i] * hkernel[j];
                kernelsum += elem;
                kernel2d[i][j] = elem;
            }
        }
        for(int i=0; i < length; i++) {
            for(int j=0; j < length; j++)
                kernel2d[i][j] /= kernelsum;
        }
        return kernel2d;
    }
}

class PPMImageHolder{
    private static PPMImage m_orig_img;

    public static void setimage(PPMImage myimage){
        m_orig_img = myimage;
    }

    public static PPMImage getimage(){
        return m_orig_img;
    }

    public static RGB[] getimageRGBarray(){
        return m_orig_img.pixels;
    }
}

