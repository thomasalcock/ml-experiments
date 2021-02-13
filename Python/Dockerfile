FROM python:latest

RUN mkdir -p /home/code
RUN mkdir -p /home/data
RUN mkdir -p /home/output

COPY /code /home/code/
WORKDIR /home/code

RUN pip install numpy
RUN pip install pandas
RUN pip install statsmodels

#CMD ["python", "ela_calc.py"]