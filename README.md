# INLAPLUS

# How to use INLAPLUS in R?

### Step 1: Download sarus (or any other software that can pull a docker image like singularity)

Installation lines for sarus - part 1
```
wget https://github.com/eth-cscs/sarus/releases/download/1.5.0/sarus-Release.tar.gz
mkdir /opt/sarus
cd /opt/sarus
sudo cp /home/<yourusername>/sarus-Release.tar.gz ./
tar xf sarus-Release.tar.gz
```
In my case I use: sudo cp /home/abdulfe/sarus-Release.tar.gz ./

If squashfs-tools not available install it
```
sudo apt-get update
sudo apt-get -y install squashfs-tools
```
or 
```
sudo apt update
sudo apt -y install squashfs-tools
```

Installation lines for sarus - part 2
```
cd /opt/sarus/1.5.0-Release
sudo ./configure_installation.sh
export PATH=/opt/sarus/1.5.0-Release/bin:${PATH}
```

Pull inlaplus docker image using sarus (this will take some time)
```
sarus pull esmailabdulfattah/inlaplus:251122
```


### Step 2: Download INLAPLUS package 

Use the following command in R to download R-INLAPLUS package
```
remotes::install_github("esmail-abdulfattah/INLAPLUS")
```

Now you can R-INLAPLUS package to run your codes. Use the R file in inst folder above to test a running code. 

# How to use INLAPLUS in Python?

1. Download the python folder above. You can go to code button and then download zip.
2. Use the user_input.py file to run the code.
3. inla1234 is a function to get the inference and plot_marginals is a function that plot the marginal posteriors of the latent field and the hyperparamater.
4. For instance: to run a poisson model, you need to have four text files in Input folder: responses (y_response.txt), offset (offset.txt - optional), precision matrix for the structured time effect (Qx_time.text) and precision matrix for the structured space effect (Qx_space.text)



