# Javier Franco
# Texas is Awesome!

import cv2
import numpy as np
#from cv2 import cv

#method = cv.CV_TM_SQDIFF_NORMED
#methods = ['cv2.TM_CCOEFF', 'cv2.TM_CCOEFF_NORMED', 'cv2.TM_CCORR',
 #           'cv2.TM_CCORR_NORMED', 'cv2.TM_SQDIFF', 'cv2.TM_SQDIFF_NORMED']


# Read the images from the file
small_image = cv2.imread('fishtemplate.png')
large_image = cv2.imread('fish2.jpg')
#large_image = cv2.resize(large_image, (725, 375)) 

orig_image=large_image.copy()

#result = cv2.matchTemplate(small_image, large_image, cv2.TM_SQDIFF_NORMED)
result = cv2.matchTemplate(small_image, large_image, cv2.TM_CCOEFF_NORMED)

# We want the minimum squared difference
mn,_,mnLoc,mxLoc = cv2.minMaxLoc(result)

# Draw the rectangle:
# Extract the coordinates of our best match
#MPx,MPy = mnLoc
MPx,MPy = mxLoc

# Step 2: Get the size of the template. This is the same size as the match.
trows,tcols = small_image.shape[:2]

# Step 3: Draw the rectangle on large_image
cv2.rectangle(large_image, (MPx,MPy),(MPx+tcols,MPy+trows),(0,0,255),2)

# Display the original image with the rectangle around the match.
cv2.imshow('output',large_image)

 #The image is only displayed if we call this
cv2.waitKey(0)

#
##%%
#
tail = large_image[MPy:MPy+trows, MPx:MPx+tcols] # Crop from x, y, w, h -> 100, 200, 300, 400
##tail= cv2.bilateralFilter(tail, 11, 17, 17)
tail = cv2.GaussianBlur(tail,(5,5),0)
## dilations to remove any small regions of noise
#tail = cv2.threshold(tail, 45, 255, cv2.THRESH_BINARY)[1]
#tail = cv2.erode(tail, None, iterations=2)
#tail = cv2.dilate(tail, None, iterations=2)
# 

cv2.imshow("Edged Tail", tail)
cv2.waitKey(0)

tail = cv2.Canny(tail, 30, 200)
#
cv2.imshow("Edged Tail", tail)
cv2.waitKey(0)


#%% Find farthest right edge
trows,tcols = small_image.shape[:2]

#Assumed that the center of image is inside the tail
xPt=round(tcols*0.60)   #Find point to start on Xaxis
yPt=round(trows/2)      #Find mid point on Yaxis

currVec=tail[:xPt,yPt] #Get top half of tail
topedgeIdx=np.max(currVec.nonzero())
currVec=tail[xPt:,yPt] #Get top half of tail
bottomedgeIdx=np.min(currVec.nonzero())+xPt

maxEdgePt=[0,0]
for currRow in range(topedgeIdx,bottomedgeIdx):
        rowVec=tail[currRow,xPt:]
        curredgeIdx=list(rowVec.nonzero())
        curredgeIdx=list(curredgeIdx[0])
        if curredgeIdx[0] >maxEdgePt[1]:
            maxEdgePt[0]=currRow
            maxEdgePt[1]=curredgeIdx[0] #assign to first nonzero value
    
    
maxEdgePt[1]=maxEdgePt[1]+xPt


tail=cv2.circle(large_image,(maxEdgePt[1]+MPx,maxEdgePt[0]+MPy), 5, (0, 0,255), -1)
tailPt=maxEdgePt[1]+MPx

cv2.imshow("Tail", tail)
cv2.waitKey(0)



#%% Find numbers

#Find edges on entire image to search for numbers
gray = cv2.cvtColor(orig_image,cv2.COLOR_BGR2GRAY)
blur = cv2.GaussianBlur(gray,(5,5),0)
edges = cv2.Canny(blur,50,150,apertureSize = 3)

#Show edges of image
#cv2.imshow("Numbers",edges)
#cv2.waitKey(0)



len_range=range(14,18)
CenterOffset=0.35

# Loop to find where the tail edge is at
for curr_len in len_range:
    currNumImage = cv2.imread(str(curr_len)+'.png')
    result = cv2.matchTemplate(currNumImage, orig_image, cv2.TM_SQDIFF_NORMED)
    if len(result) != 0:
        mn,_,mnLoc,_ = cv2.minMaxLoc(result)
        MPx,MPy = mnLoc
        trows,tcols = currNumImage.shape[:2]
        cv2.rectangle(large_image, (MPx,MPy),(MPx+tcols,MPy+trows),(0,0,255),2)
        maxCenter=MPx+tcols*CenterOffset
        if maxCenter > tailPt:
            print('The hi range has been found.')            
            break

lowrange=curr_len-1
currNumImage = cv2.imread(str(lowrange)+'.png')
result = cv2.matchTemplate(currNumImage, orig_image, cv2.TM_SQDIFF_NORMED)
mn,_,mnLoc,_ = cv2.minMaxLoc(result)
MPx,MPy = mnLoc
trows,tcols = currNumImage.shape[:2]
cv2.rectangle(large_image, (MPx,MPy),(MPx+tcols,MPy+trows),(0,0,255),2)
minCenter=MPx+tcols*CenterOffset


#cv2.imwrite('houghlines3.jpg',large_image)
cv2.imshow("Numbers",large_image)
cv2.waitKey(0)


## Make some location calcs
numSpan=maxCenter-minCenter
ptLoc=maxCenter-tailPt
ptFrac=1-ptLoc/numSpan

finalLength=16+ptFrac
print(finalLength)


