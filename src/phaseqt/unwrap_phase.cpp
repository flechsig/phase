 // File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/unwrap_phase.cpp
 // Date      : <26 Mar 14 09:31:57 flechsig> 
 // Time-stamp: <26 Mar 14 09:34:59 flechsig> 
 // Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 // $Source$ 
 // $Date$
 // $Revision$ 
 // $Author$ 

// UF example code found on http://www.ljmu.ac.uk/GERI/CEORG_Docs/Miguel_2D_unwrapper.cpp
// code will be tested to do the 2d phase unwrapping

//This program is written by Munther Gdeisat and Miguel Arevallilo Herra´ez to program the two-dimensional unwrapper
//entitled "Fast two-dimensional phase-unwrapping algorithm based on sorting by 
//reliability following a noncontinuous path"
//by  Miguel Arevallilo Herra´ez, David R. Burton, Michael J. Lalor, and Munther A. Gdeisat
//published in the Applied Optics, Vol. 41, No. 35, pp. 7437, 2002.
//This program is written on 15th August 2007
//The wrapped phase map is floating point data type. Also, the unwrapped phase map is foloating point
#include <malloc.h>
#include "stdafx.h"
#include<stdio.h>
#include <stdlib.h>
#include <string.h>

static float PI = 3.141592654;
static float TWOPI = 6.283185307;

//pixel information
struct PIXEL
{
	//int x;					//x coordinate of the pixel
    //int y;					//y coordinate
    int increment;			//No. of 2*pi to add to the pixel to unwrap it
    int number_of_pixels_in_group;	//No. of pixels in the pixel group
    float value;			//value of the pixel
	float reliability;
    int group;				//group No.
    int new_group;
    struct PIXEL *head;		//pointer to the first pixel in the group in the linked list
    struct PIXEL *last;		//pointer to the last pixel in the group
    struct PIXEL *next;		//pointer to the next pixel in the group
};


//the EDGE is the line that connects two pixels.
//if we have S PIXELs, then we have S horizental edges and S vertical edges
struct EDGE
{    
	float reliab;			//reliabilty of the edge and it depends on the two pixels
	PIXEL *pointer_1;		//pointer to the first pixel
    PIXEL *pointer_2;		//pointer to the second pixel
    int increment;			//No. of 2*pi to add to one of the pixels to unwrap it with respect to the second 
}; 


//another version of Mixtogether but this function should only be use with the sort program
void  Mix(EDGE *Pointer1, int *index1, int *index2, int size)
{
	int counter1 = 0;
	int counter2 = 0;
	int *TemporalPointer = index1;

	int *Result = (int *) calloc(size * 2, sizeof(int));
	int *Follower = Result;

	while ((counter1 < size) && (counter2 < size))
	{
		if ((Pointer1[*(index1 + counter1)].reliab <= Pointer1[*(index2 + counter2)].reliab))
		{
			*Follower = *(index1 + counter1);
			Follower++;
			counter1++;
		} 
		else
        {
			*Follower = *(index2 + counter2);
			Follower++;
			counter2++;
        }
	}//while

	if (counter1 == size)
	{
		memcpy(Follower, (index2 + counter2), sizeof(int)*(size-counter2));
	} 
	else
	{
		memcpy(Follower, (index1 + counter1), sizeof(int)*(size-counter1));
	}

	Follower = Result;
	index1 = TemporalPointer;

	int i;
	for (i=0; i < 2 * size; i++)
	{
		*index1 = *Follower;
		index1++;
		Follower++;
	}

	free(Result);
}

//this is may be the fastest sort program; 
//see the explination in quickSort function below
void  sort(EDGE *Pointer, int *index, int size)
{
	if (size == 2)
	{
		if ((Pointer[*index].reliab) > (Pointer[*(index+1)].reliab))
		{
			int Temp;
			Temp = *index;
			*index = *(index+1);
			*(index+1) = Temp;
		}
	} 
	else if (size > 2)
    {
		sort(Pointer, index, size/2);
		sort(Pointer, (index + (size/2)), size/2);
		Mix(Pointer, index, (index + (size/2)), size/2);
    }
}

//this function tries to implement a nice idea explained below
//we need to sort edge array. Each edge element conisists of 16 bytes.
//In normal sort program we compare two elements in the array and exchange
//their place under some conditions to do the sorting. It is very probable
// that an edge element may change its place hundred of times which makes 
//the sorting a very time consuming operation. The idea in this function 
//is to give each edge element an index and move the index not the edge
//element. The edge need 4 bytes which makes the sorting operation faster.
// After finishingthe sorting of the indexes, we know the position of each index. 
//So we know how to sort edges
void  quick_sort(EDGE *Pointer, int size)
{
	int *index = (int *) calloc(size, sizeof(int));
	int i;

	for (i=0; i<size; ++i)
		index[i] = i;

	sort(Pointer, index, size);

	EDGE * a = (EDGE *) calloc(size, sizeof(EDGE));
	for (i=0; i<size; ++i)
		a[i] = Pointer[*(index + i)];

	memcpy(Pointer, a, size*sizeof(EDGE));

	free(index);
	free(a);
}



void read_data(char *inputfile,float *Data, int length)
{
	printf("Reading the Wrapped Values form Binary File.............>");
	FILE *ifptr;
	ifptr = fopen(inputfile,"rb");
	if(ifptr == NULL) printf("Error opening the file\n");
	fread(Data,sizeof(float),length,ifptr);
	fclose(ifptr);
	printf(" Done.\n");
}

void write_data(char *outputfile,float *Data,int length)
{
	printf("Writing the Unwrapped Values to Binary File.............>");
	FILE *ifptr;
	ifptr = fopen(outputfile,"wb");
	if(ifptr == NULL) printf("Error opening the file\n");
	fwrite(Data,sizeof(float),length,ifptr);
	fclose(ifptr);
	printf(" Done.\n");
}

//---------------start quicker_sort algorithm --------------------------------
#define swap(x,y) {EDGE t; t=x; x=y; y=t;}
#define order(x,y) if (x.reliab > y.reliab) swap(x,y)
#define o2(x,y) order(x,y)
#define o3(x,y,z) o2(x,y); o2(x,z); o2(y,z)

typedef enum {yes, no} yes_no;

yes_no find_pivot(EDGE *left, EDGE *right, float *pivot_ptr)
{
	EDGE a, b, c, *p;

	a = *left;
	b = *(left + (right - left) /2 );
	c = *right;
	o3(a,b,c);

	if (a.reliab < b.reliab)
	{
		*pivot_ptr = b.reliab;
		return yes;
	}

	if (b.reliab < c.reliab)
	{
		*pivot_ptr = c.reliab;
		return yes;
	}

	for (p = left + 1; p <= right; ++p)
	{
		if (p->reliab != left->reliab)
		{
			*pivot_ptr = (p->reliab < left->reliab) ? left->reliab : p->reliab;
			return yes;
		}
		return no;
	}
}

EDGE *partition(EDGE *left, EDGE *right, float pivot)
{
	while (left <= right)
	{
		while (left->reliab < pivot)
			++left;
		while (right->reliab >= pivot)
			--right;
		if (left < right)
		{
			swap (*left, *right);
			++left;
			--right;
		}
	}
	return left;
}

void quicker_sort(EDGE *left, EDGE *right)
{
	EDGE *p;
	float pivot;

	if (find_pivot(left, right, &pivot) == yes)
	{
		p = partition(left, right, pivot);
		quicker_sort(left, p - 1);
		quicker_sort(p, right);
	}
}

//--------------end quicker_sort algorithm -----------------------------------

//--------------------start initialse pixels ----------------------------------
//initialse pixels. See the explination of the pixel class above.
//initially every pixel is a gorup by its self
void  initialisePIXELs(float *WrappedImage, PIXEL *pixel, int image_width, int image_height)
{
	PIXEL *pixel_pointer = pixel;
	float *wrapped_image_pointer = WrappedImage;
	int i, j;

    for (i=0; i < image_height; i++)
	{
		for (j=0; j < image_width; j++)
        {
			//pixel_pointer->x = j;
  			//pixel_pointer->y = i;
			pixel_pointer->increment = 0;
			pixel_pointer->number_of_pixels_in_group = 1;		
  			pixel_pointer->value = *wrapped_image_pointer;
			pixel_pointer->reliability = 9999999+rand();
			pixel_pointer->head = pixel_pointer;
  			pixel_pointer->last = pixel_pointer;
			pixel_pointer->next = NULL;			
            pixel_pointer->new_group = 0;
            pixel_pointer->group = -1;
            pixel_pointer++;
            wrapped_image_pointer++;
         }
	}
}
//-------------------end initialise pixels -----------

//gamma function in the paper
float wrap(float pixel_value)
{
	float wrapped_pixel_value;
	if (pixel_value > PI)	wrapped_pixel_value = pixel_value - TWOPI;
	else if (pixel_value < -PI)	wrapped_pixel_value = pixel_value + TWOPI;
	else wrapped_pixel_value = pixel_value;
	return wrapped_pixel_value;
}

// pixelL_value is the left pixel,	pixelR_value is the right pixel
int find_wrap(float pixelL_value, float pixelR_value)
{
	float difference; 
	int wrap_value;
	difference = pixelL_value - pixelR_value;

	if (difference > PI)	wrap_value = -1;
	else if (difference < -PI)	wrap_value = 1;
	else wrap_value = 0;

	return wrap_value;
} 

void calculate_reliability(float *wrappedImage, PIXEL *pixel, int image_width, int image_height)
{
	int image_width_plus_one = image_width + 1;
	int image_width_minus_one = image_width - 1;
	PIXEL *pixel_pointer = pixel + image_width_plus_one;
	float *WIP = wrappedImage + image_width_plus_one; //WIP is the wrapped image pointer
	float H, V, D1, D2;
	int i, j;
	
	for (i = 1; i < image_height -1; ++i)
	{
		for (j = 1; j < image_width - 1; ++j)
		{
			H = wrap(*(WIP - 1) - *WIP) - wrap(*WIP - *(WIP + 1));
			V = wrap(*(WIP - image_width) - *WIP) - wrap(*WIP - *(WIP + image_width));
			D1 = wrap(*(WIP - image_width_plus_one) - *WIP) - wrap(*WIP - *(WIP + image_width_plus_one));
			D2 = wrap(*(WIP - image_width_minus_one) - *WIP) - wrap(*WIP - *(WIP + image_width_minus_one));
			pixel_pointer->reliability = H*H + V*V + D1*D1 + D2*D2;
			pixel_pointer++;
			WIP++;
		}
		pixel_pointer += 2;
		WIP += 2;
	}
}

//calculate the reliability of the horizental edges of the image
//it is calculated by adding the reliability of pixel and the relibility of 
//its right neighbour
//edge is calculated between a pixel and its next neighbour
void  horizentalEDGEs(PIXEL *pixel, EDGE *edge, int image_width, int image_height)
{
	int i, j;
	EDGE *edge_pointer = edge;
	PIXEL *pixel_pointer = pixel;
	
	for (i = 0; i < image_height; i++)
	{
		for (j = 0; j < image_width - 1; j++) 
		{
			edge_pointer->pointer_1 = pixel_pointer;
			edge_pointer->pointer_2 = (pixel_pointer+1);
			edge_pointer->reliab = pixel_pointer->reliability + (pixel_pointer + 1)->reliability;
			edge_pointer->increment = find_wrap(pixel_pointer->value, (pixel_pointer + 1)->value);
			pixel_pointer++;
			edge_pointer++;
		}
		pixel_pointer++;
	}
}

//calculate the reliability of the vertical EDGEs of the image
//it is calculated by adding the reliability of pixel and the relibility of 
//its lower neighbour in the image.
void  verticalEDGEs(PIXEL *pixel, EDGE *edge, int image_width, int image_height)
{
	int i, j;
	
	PIXEL *pixel_pointer = pixel;
	EDGE *edge_pointer = edge + (image_height) * (image_width - 1); 

	for (i=0; i<image_height - 1; i++)
	{
		for (j=0; j < image_width; j++) 
		{
			edge_pointer->pointer_1 = pixel_pointer;
			edge_pointer->pointer_2 = (pixel_pointer + image_width);
			edge_pointer->reliab = pixel_pointer->reliability + (pixel_pointer + image_width)->reliability;
			edge_pointer->increment = find_wrap(pixel_pointer->value, (pixel_pointer + image_width)->value);
			pixel_pointer++;
			edge_pointer++;
		} //j loop
	} // i loop
}

//gather the pixels of the image into groups 
void  gatherPIXELs(EDGE *edge, int image_width, int image_height)
{
	int k;
	
	//Number of rialiable edges (not at the borders of the image)
	int no_EDGEs = (image_width - 1) * (image_height) + (image_width) * (image_height - 1); 
	PIXEL *PIXEL1;   
	PIXEL *PIXEL2;

	PIXEL *group1;
	PIXEL *group2;
	EDGE *pointer_edge = edge;
	int incremento;

	for (k = 0; k < no_EDGEs; k++)
	{
		PIXEL1 = pointer_edge->pointer_1;
		PIXEL2 = pointer_edge->pointer_2;

		//PIXEL 1 and PIXEL 2 belong to different groups
		//initially each pixel is a group by it self and one pixel can construct a group
		//no else or else if to this if
		if (PIXEL2->head != PIXEL1->head)
		{
			//PIXEL 2 is alone in its group
			//merge this pixel with PIXEL 1 group and find the number of 2 pi to add 
			//to or subtract to unwrap it
			if ((PIXEL2->next == NULL) && (PIXEL2->head == PIXEL2))
			{
				PIXEL1->head->last->next = PIXEL2;
				PIXEL1->head->last = PIXEL2;
				(PIXEL1->head->number_of_pixels_in_group)++;
				PIXEL2->head=PIXEL1->head;
				PIXEL2->increment = PIXEL1->increment-pointer_edge->increment;
			}

			//PIXEL 1 is alone in its group
			//merge this pixel with PIXEL 2 group and find the number of 2 pi to add 
			//to or subtract to unwrap it
			else if ((PIXEL1->next == NULL) && (PIXEL1->head == PIXEL1))
			{
				PIXEL2->head->last->next = PIXEL1;
				PIXEL2->head->last = PIXEL1;
				(PIXEL2->head->number_of_pixels_in_group)++;
				PIXEL1->head = PIXEL2->head;
				PIXEL1->increment = PIXEL2->increment+pointer_edge->increment;
			} 

			//PIXEL 1 and PIXEL 2 both have groups
			else
            {
				group1 = PIXEL1->head;
                group2 = PIXEL2->head;
				//the no. of pixels in PIXEL 1 group is large than the no. of PIXELs
				//in PIXEL 2 group.   Merge PIXEL 2 group to PIXEL 1 group
				//and find the number of wraps between PIXEL 2 group and PIXEL 1 group
				//to unwrap PIXEL 2 group with respect to PIXEL 1 group.
				//the no. of wraps will be added to PIXEL 2 grop in the future
				if (group1->number_of_pixels_in_group > group2->number_of_pixels_in_group)
				{
					//merge PIXEL 2 with PIXEL 1 group
					group1->last->next = group2;
					group1->last = group2->last;
					group1->number_of_pixels_in_group = group1->number_of_pixels_in_group + group2->number_of_pixels_in_group;
					incremento = PIXEL1->increment-pointer_edge->increment - PIXEL2->increment;
					//merge the other pixels in PIXEL 2 group to PIXEL 1 group
					while (group2 != NULL)
					{
						group2->head = group1;
						group2->increment += incremento;
						group2 = group2->next;
					}
				} 

				//the no. of PIXELs in PIXEL 2 group is large than the no. of PIXELs
				//in PIXEL 1 group.   Merge PIXEL 1 group to PIXEL 2 group
				//and find the number of wraps between PIXEL 2 group and PIXEL 1 group
				//to unwrap PIXEL 1 group with respect to PIXEL 2 group.
				//the no. of wraps will be added to PIXEL 1 grop in the future
				else
                {
					//merge PIXEL 1 with PIXEL 2 group
					group2->last->next = group1;
					group2->last = group1->last;
					group2->number_of_pixels_in_group = group2->number_of_pixels_in_group + group1->number_of_pixels_in_group;
					incremento = PIXEL2->increment + pointer_edge->increment - PIXEL1->increment;
					//merge the other pixels in PIXEL 2 group to PIXEL 1 group
					while (group1 != NULL)
					{
						group1->head = group2;
						group1->increment += incremento;
						group1 = group1->next;
					} // while

                } // else
            } //else
        } ;//if

        pointer_edge++;
	}
}

//unwrap the image 
void  unwrapImage(PIXEL *pixel, int image_width, int image_height)
{
	int i;
	int image_size = image_width * image_height;
	PIXEL *pixel_pointer=pixel;

	for (i = 0; i < image_size; i++)
	{
		pixel_pointer->value += TWOPI * (float)(pixel_pointer->increment);
        pixel_pointer++;
    }
}

//the input to this unwrapper is an array that contains the wrapped phase map. 
//copy the image on the buffer passed to this unwrapper to over write the unwrapped 
//phase map on the buffer of the wrapped phase map.
void  returnImage(PIXEL *pixel, float *unwrappedImage, int image_width, int image_height)
{
	int i;
	int image_size = image_width * image_height;
    float *unwrappedImage_pointer = unwrappedImage;
    PIXEL *pixel_pointer = pixel;

    for (i=0; i < image_size; i++) 
	{
        *unwrappedImage_pointer = pixel_pointer->value;
        pixel_pointer++;
		unwrappedImage_pointer++;
	}
}


//the main fnction of the unwrapper
int main()
{  
	float *WrappedImage, *UnwrappedImage;
	int image_width = 512;
	int image_height = 512;

	int i, j; 
	int image_size = image_height * image_width;
	int two_image_size = 2 * image_size;

	int No_of_Edges = (image_width)*(image_height-1) + (image_width-1)*(image_height);

	WrappedImage = (float *) calloc(image_size, sizeof(float));
	read_data("wrapped phase map 512X512a.dat",WrappedImage,image_size);

	UnwrappedImage = (float *) calloc(image_size, sizeof(float));
	PIXEL *pixel = (PIXEL *) calloc(image_size, sizeof(PIXEL));
	EDGE *edge = (EDGE *) calloc(No_of_Edges, sizeof(EDGE));;

	//initialise the pixels
	initialisePIXELs(WrappedImage, pixel, image_width, image_height);

	calculate_reliability(WrappedImage, pixel, image_width, image_height);

	horizentalEDGEs(pixel, edge, image_width, image_height);

	verticalEDGEs(pixel, edge, image_width, image_height);

	//sort the EDGEs depending on their reiability. The PIXELs with higher relibility (small value) first
	//if your code stuck because of the quicker_sort() function, then use the quick_sort() function
	//run only one of the two functions (quick_sort() or quicker_sort() )
	//quick_sort(edge, No_of_Edges);
	quicker_sort(edge, edge + No_of_Edges - 1);
	
	//gather PIXELs into groups
	gatherPIXELs(edge, image_width, image_height);


	//unwrap the whole image
	unwrapImage(pixel, image_width, image_height);
	//copy the image from PIXEL structure to the wrapped phase array passed to this function
	returnImage(pixel, UnwrappedImage, image_width, image_height);

	free(edge);
	free(pixel);
	
	write_data("unwrapped.dat",UnwrappedImage,image_size);
	free(UnwrappedImage);
	free(WrappedImage);
	getchar();
	return 1;
}
// end
